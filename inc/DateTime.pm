#line 1
package DateTime;

use 5.006;

use strict;
use warnings;

use Carp;
use DateTime::Helpers;

our $VERSION;

BEGIN
{
    $VERSION = '0.53';

    my $loaded = 0;
    unless ( $ENV{PERL_DATETIME_PP} )
    {
        local $@;
	eval
	{
            require XSLoader;
            XSLoader::load( 'DateTime', $DateTime::VERSION );

            $DateTime::IsPurePerl = 0;
	};

	die $@ if $@ && $@ !~ /object version|loadable object/;

        $loaded = 1 unless $@;
    }

    if ($loaded)
    {
        require DateTimePPExtra
            unless defined &DateTime::_normalize_tai_seconds;
    }
    else
    {
        require DateTimePP;
    }
}

use DateTime::Duration;
use DateTime::Locale 0.40;
use DateTime::TimeZone 0.59;
use Time::Local qw( timegm_nocheck );
use Params::Validate qw( validate validate_pos SCALAR BOOLEAN HASHREF OBJECT );

# for some reason, overloading doesn't work unless fallback is listed
# early.
#
# 3rd parameter ( $_[2] ) means the parameters are 'reversed'.
# see: "Calling conventions for binary operations" in overload docs.
#
use overload ( 'fallback' => 1,
               '<=>' => '_compare_overload',
               'cmp' => '_compare_overload',
               '""'  => '_stringify',
               '-'   => '_subtract_overload',
               '+'   => '_add_overload',
               'eq'  => '_string_equals_overload',
               'ne'  => '_string_not_equals_overload',
             );

# Have to load this after overloading is defined, after BEGIN blocks
# or else weird crashes ensue
require DateTime::Infinite;

use constant MAX_NANOSECONDS => 1_000_000_000;  # 1E9 = almost 32 bits

use constant INFINITY     =>      (9 ** 9 ** 9);
use constant NEG_INFINITY => -1 * (9 ** 9 ** 9);
use constant NAN          => INFINITY - INFINITY;

use constant SECONDS_PER_DAY => 86400;

use constant duration_class => 'DateTime::Duration';

my( @MonthLengths, @LeapYearMonthLengths );

BEGIN
{
    @MonthLengths =
        ( 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 );

    @LeapYearMonthLengths = @MonthLengths;
    $LeapYearMonthLengths[1]++;
}

{
    # I'd rather use Class::Data::Inheritable for this, but there's no
    # way to add the module-loading behavior to an accessor it
    # creates, despite what its docs say!
    my $DefaultLocale;
    sub DefaultLocale
    {
        my $class = shift;

        if (@_)
        {
            my $lang = shift;

            DateTime::Locale->load($lang);

            $DefaultLocale = $lang;
        }

        return $DefaultLocale;
    }
    # backwards compat
    *DefaultLanguage = \&DefaultLocale;
}
__PACKAGE__->DefaultLocale('en_US');

my $BasicValidate =
    { year   => { type => SCALAR,
                  callbacks =>
                  { 'is an integer' =>
                    sub { $_[0] =~ /^-?\d+$/ }
                  },
                },
      month  => { type => SCALAR, default => 1,
                  callbacks =>
                  { 'an integer between 1 and 12' =>
                    sub { $_[0] =~ /^\d+$/ && $_[0] >= 1 && $_[0] <= 12 }
                  },
                },
      day    => { type => SCALAR, default => 1,
                  callbacks =>
                  { 'an integer which is a possible valid day of month' =>
                    sub { $_[0] =~ /^\d+$/ && $_[0] >= 1 && $_[0] <= 31 }
                  },
                },
      hour   => { type => SCALAR, default => 0,
                  callbacks =>
                  { 'an integer between 0 and 23' =>
                    sub { $_[0] =~ /^\d+$/ && $_[0] >= 0 && $_[0] <= 23 },
                  },
                },
      minute => { type => SCALAR, default => 0,
                  callbacks =>
                  { 'an integer between 0 and 59' =>
                    sub { $_[0] =~ /^\d+$/ && $_[0] >= 0 && $_[0] <= 59 },
                  },
                },
      second => { type => SCALAR, default => 0,
                  callbacks =>
                  { 'an integer between 0 and 61' =>
                    sub { $_[0] =~ /^\d+$/ && $_[0] >= 0 && $_[0] <= 61 },
                  },
                },
      nanosecond => { type => SCALAR, default => 0,
                      callbacks =>
                      { 'a positive integer' =>
                        sub { $_[0] =~ /^\d+$/ && $_[0] >= 0 },
                      }
                    },
      locale    => { type => SCALAR | OBJECT,
                     default => undef },
      language  => { type => SCALAR | OBJECT,
                     optional => 1 },
    };

my $NewValidate =
    { %$BasicValidate,
      time_zone => { type => SCALAR | OBJECT,
                     default => 'floating' },
      formatter => { type => SCALAR | OBJECT, can => 'format_datetime', optional => 1 },
    };

sub new
{
    my $class = shift;
    my %p = validate( @_, $NewValidate );

    Carp::croak( "Invalid day of month (day = $p{day} - month = $p{month} - year = $p{year})\n" )
        if $p{day} > $class->_month_length( $p{year}, $p{month} );

    my $self = bless {}, $class;

    $p{locale} = delete $p{language} if exists $p{language};
    $p{locale} = $class->DefaultLocale unless defined $p{locale};

    if ( ref $p{locale} )
    {
        $self->{locale} = $p{locale};
    }
    else
    {
        $self->{locale} = DateTime::Locale->load( $p{locale} );
    }

    $self->{tz} =
        ( ref $p{time_zone} ?
          $p{time_zone} :
          DateTime::TimeZone->new( name => $p{time_zone} )
        );

    $self->{local_rd_days} =
        $class->_ymd2rd( @p{ qw( year month day ) } );

    $self->{local_rd_secs} =
        $class->_time_as_seconds( @p{ qw( hour minute second ) } );

    $self->{offset_modifier} = 0;

    $self->{rd_nanosecs} = $p{nanosecond};
    $self->{formatter} = $p{formatter};

    $self->_normalize_nanoseconds( $self->{local_rd_secs}, $self->{rd_nanosecs} );

    # Set this explicitly since it can't be calculated accurately
    # without knowing our time zone offset, and it's possible that the
    # offset can't be calculated without having at least a rough guess
    # of the datetime's year.  This year need not be correct, as long
    # as its equal or greater to the correct number, so we fudge by
    # adding one to the local year given to the constructor.
    $self->{utc_year} = $p{year} + 1;

    $self->_calc_utc_rd;

    $self->_handle_offset_modifier( $p{second} );

    $self->_calc_local_rd;

    if ( $p{second} > 59 )
    {
        if ( $self->{tz}->is_floating ||
             # If true, this means that the actual calculated leap
             # second does not occur in the second given to new()
             ( $self->{utc_rd_secs} - 86399
               <
               $p{second} - 59 )
           )
        {
            Carp::croak( "Invalid second value ($p{second})\n" );
        }
    }

    return $self;
}

# This method exists for the benefit of internal methods which create
# a new object based on the current object, like set() and truncate().
sub _new_from_self
{
    my $self = shift;

    my %old = map { $_ => $self->$_() }
        qw( year month day hour minute second nanosecond
            locale time_zone );
    $old{formatter} = $self->formatter()
        if defined $self->formatter();

    return (ref $self)->new( %old, @_ );
}

sub _handle_offset_modifier
{
    my $self = shift;

    $self->{offset_modifier} = 0;

    return if $self->{tz}->is_floating;

    my $second = shift;
    my $utc_is_valid = shift;

    my $utc_rd_days = $self->{utc_rd_days};

    my $offset = $utc_is_valid ? $self->offset : $self->_offset_for_local_datetime;

    if ( $offset >= 0
         && $self->{local_rd_secs} >= $offset
       )
    {
        if ( $second < 60 && $offset > 0 )
        {
            $self->{offset_modifier} =
                $self->_day_length( $utc_rd_days - 1 ) - SECONDS_PER_DAY;

            $self->{local_rd_secs} += $self->{offset_modifier};
        }
        elsif ( $second == 60
                &&
                ( ( $self->{local_rd_secs} == $offset
                    && $offset > 0 )
                  ||
                  ( $offset == 0
                    && $self->{local_rd_secs} > 86399 ) )
              )
        {
            my $mod = $self->_day_length( $utc_rd_days - 1 ) - SECONDS_PER_DAY;

            unless ( $mod == 0 )
            {
                $self->{utc_rd_secs} -= $mod;

                $self->_normalize_seconds;
            }
        }
    }
    elsif ( $offset < 0
            && $self->{local_rd_secs} >= SECONDS_PER_DAY + $offset )
    {
        if ( $second < 60 )
        {
            $self->{offset_modifier} =
                $self->_day_length( $utc_rd_days - 1 ) - SECONDS_PER_DAY;

            $self->{local_rd_secs} += $self->{offset_modifier};
        }
        elsif ( $second == 60 && $self->{local_rd_secs} == SECONDS_PER_DAY + $offset )
        {
            my $mod = $self->_day_length( $utc_rd_days - 1 ) - SECONDS_PER_DAY;

            unless ( $mod == 0 )
            {
                $self->{utc_rd_secs} -= $mod;

                $self->_normalize_seconds;
            }
        }
    }
}

sub _calc_utc_rd
{
    my $self = shift;

    delete $self->{utc_c};

    if ( $self->{tz}->is_utc || $self->{tz}->is_floating )
    {
        $self->{utc_rd_days} = $self->{local_rd_days};
        $self->{utc_rd_secs} = $self->{local_rd_secs};
    }
    else
    {
        my $offset = $self->_offset_for_local_datetime;

        $offset += $self->{offset_modifier};

        $self->{utc_rd_days} = $self->{local_rd_days};
        $self->{utc_rd_secs} = $self->{local_rd_secs} - $offset;
    }

    # We account for leap seconds in the new() method and nowhere else
    # except date math.
    $self->_normalize_tai_seconds( $self->{utc_rd_days}, $self->{utc_rd_secs} );
}

sub _normalize_seconds
{
    my $self = shift;

    return if $self->{utc_rd_secs} >= 0 && $self->{utc_rd_secs} <= 86399;

    if ( $self->{tz}->is_floating )
    {
        $self->_normalize_tai_seconds( $self->{utc_rd_days}, $self->{utc_rd_secs} );
    }
    else
    {
        $self->_normalize_leap_seconds( $self->{utc_rd_days}, $self->{utc_rd_secs} );
    }
}

sub _calc_local_rd
{
    my $self = shift;

    delete $self->{local_c};

    # We must short circuit for UTC times or else we could end up with
    # loops between DateTime.pm and DateTime::TimeZone
    if ( $self->{tz}->is_utc || $self->{tz}->is_floating )
    {
        $self->{local_rd_days} = $self->{utc_rd_days};
        $self->{local_rd_secs} = $self->{utc_rd_secs};
    }
    else
    {
        my $offset = $self->offset;

        $self->{local_rd_days} = $self->{utc_rd_days};
        $self->{local_rd_secs} = $self->{utc_rd_secs} + $offset;

        # intentionally ignore leap seconds here
        $self->_normalize_tai_seconds( $self->{local_rd_days}, $self->{local_rd_secs} );

        $self->{local_rd_secs} += $self->{offset_modifier};
    }

    $self->_calc_local_components;
}

sub _calc_local_components
{
    my $self = shift;

    @{ $self->{local_c} }{ qw( year month day day_of_week
                               day_of_year quarter day_of_quarter) } =
        $self->_rd2ymd( $self->{local_rd_days}, 1 );

    @{ $self->{local_c} }{ qw( hour minute second ) } =
        $self->_seconds_as_components
            ( $self->{local_rd_secs}, $self->{utc_rd_secs}, $self->{offset_modifier} );
}

sub _calc_utc_components
{
    my $self = shift;

    die "Cannot get UTC components before UTC RD has been calculated\n"
        unless defined $self->{utc_rd_days};

    @{ $self->{utc_c} }{ qw( year month day ) } =
        $self->_rd2ymd( $self->{utc_rd_days} );

    @{ $self->{utc_c} }{ qw( hour minute second ) } =
        $self->_seconds_as_components( $self->{utc_rd_secs} );
}

sub _utc_ymd
{
    my $self = shift;

    $self->_calc_utc_components unless exists $self->{utc_c}{year};

    return @{ $self->{utc_c} }{ qw( year month day ) };
}

sub _utc_hms
{
    my $self = shift;

    $self->_calc_utc_components unless exists $self->{utc_c}{hour};

    return @{ $self->{utc_c} }{ qw( hour minute second ) };
}

{
    my $spec = { epoch      => { regex => qr/^-?(?:\d+(?:\.\d*)?|\.\d+)$/ },
                 locale     => { type => SCALAR | OBJECT, optional => 1 },
                 language   => { type => SCALAR | OBJECT, optional => 1 },
                 time_zone  => { type => SCALAR | OBJECT, optional => 1 },
                 formatter  => { type => SCALAR | OBJECT, can => 'format_datetime',
                                 optional => 1 },
               };

    sub from_epoch
    {
        my $class = shift;
        my %p = validate( @_, $spec );

        my %args;

        # Because epoch may come from Time::HiRes
        my $fraction = $p{epoch} - int( $p{epoch} );
        $args{nanosecond} = int( $fraction * MAX_NANOSECONDS )
            if $fraction;

        # Note, for very large negative values this may give a
        # blatantly wrong answer.
        @args{ qw( second minute hour day month year ) } =
            ( gmtime( int delete $p{epoch} ) )[ 0..5 ];
        $args{year} += 1900;
        $args{month}++;

        my $self = $class->new( %p, %args, time_zone => 'UTC' );

        $self->set_time_zone( $p{time_zone} ) if exists $p{time_zone};

        return $self;
    }
}

# use scalar time in case someone's loaded Time::Piece
sub now { shift->from_epoch( epoch => (scalar time), @_ ) }

sub today { shift->now(@_)->truncate( to => 'day' ) }

{
    my $spec = { object => { type => OBJECT,
                             can => 'utc_rd_values',
                           },
                 locale     => { type => SCALAR | OBJECT, optional => 1 },
                 language   => { type => SCALAR | OBJECT, optional => 1 },
                 formatter  => { type => SCALAR | OBJECT, can => 'format_datetime',
                                 optional => 1 },
               };

    sub from_object
    {
        my $class = shift;
        my %p = validate( @_, $spec );

        my $object = delete $p{object};

        my ( $rd_days, $rd_secs, $rd_nanosecs ) = $object->utc_rd_values;

        # A kludge because until all calendars are updated to return all
        # three values, $rd_nanosecs could be undef
        $rd_nanosecs ||= 0;

        # This is a big hack to let _seconds_as_components operate naively
        # on the given value.  If the object _is_ on a leap second, we'll
        # add that to the generated seconds value later.
        my $leap_seconds = 0;
        if ( $object->can('time_zone') && ! $object->time_zone->is_floating
             && $rd_secs > 86399 && $rd_secs <= $class->_day_length($rd_days) )
        {
            $leap_seconds = $rd_secs - 86399;
            $rd_secs -= $leap_seconds;
        }

        my %args;
        @args{ qw( year month day ) } = $class->_rd2ymd($rd_days);
        @args{ qw( hour minute second ) } =
            $class->_seconds_as_components($rd_secs);
        $args{nanosecond} = $rd_nanosecs;

        $args{second} += $leap_seconds;

        my $new = $class->new( %p, %args, time_zone => 'UTC' );

        if ( $object->can('time_zone') )
        {
            $new->set_time_zone( $object->time_zone );
        }
        else
        {
            $new->set_time_zone( 'floating' );
        }

        return $new;
    }
}

my $LastDayOfMonthValidate = { %$NewValidate };
foreach ( keys %$LastDayOfMonthValidate )
{
    my %copy = %{ $LastDayOfMonthValidate->{$_} };

    delete $copy{default};
    $copy{optional} = 1 unless $_ eq 'year' || $_ eq 'month';

    $LastDayOfMonthValidate->{$_} = \%copy;
}

sub last_day_of_month
{
    my $class = shift;
    my %p = validate( @_, $LastDayOfMonthValidate );

    my $day = $class->_month_length( $p{year}, $p{month} );

    return $class->new( %p, day => $day );
}

sub _month_length
{
    return ( $_[0]->_is_leap_year( $_[1] ) ?
             $LeapYearMonthLengths[ $_[2] - 1 ] :
             $MonthLengths[ $_[2] - 1 ]
           );
}

my $FromDayOfYearValidate = { %$NewValidate };
foreach ( keys %$FromDayOfYearValidate )
{
    next if $_ eq 'month' || $_ eq 'day';

    my %copy = %{ $FromDayOfYearValidate->{$_} };

    delete $copy{default};
    $copy{optional} = 1 unless $_ eq 'year' || $_ eq 'month';

    $FromDayOfYearValidate->{$_} = \%copy;
}
$FromDayOfYearValidate->{day_of_year} =
    { type => SCALAR,
      callbacks =>
      { 'is between 1 and 366' =>
        sub { $_[0] >= 1 && $_[0] <= 366 }
      }
    };
sub from_day_of_year
{
    my $class = shift;
    my %p = validate( @_, $FromDayOfYearValidate );

    my $is_leap_year = $class->_is_leap_year( $p{year} );

    Carp::croak( "$p{year} is not a leap year.\n" )
        if $p{day_of_year} == 366 && ! $is_leap_year;

    my $month = 1;
    my $day = delete $p{day_of_year};

    while ( $month <= 12 && $day > $class->_month_length( $p{year}, $month ) )
    {
        $day -= $class->_month_length( $p{year}, $month );
        $month++;
    }

    return DateTime->new( %p,
                          month => $month,
                          day   => $day,
                        );
}

sub formatter { $_[0]->{formatter} }

sub clone { bless { %{ $_[0] } }, ref $_[0] }

sub year {
    Carp::carp('year() is a read-only accessor') if @_ > 1;
    return $_[0]->{local_c}{year};
}

sub ce_year { $_[0]->{local_c}{year} <= 0 ?
              $_[0]->{local_c}{year} - 1 :
              $_[0]->{local_c}{year} }

sub era_name { $_[0]->{locale}->era_wide->[ $_[0]->_era_index() ] }

sub era_abbr { $_[0]->{locale}->era_abbreviated->[ $_[0]->_era_index() ] }
# deprecated
*era = \&era_abbr;

sub _era_index { $_[0]->{local_c}{year} <= 0 ? 0 : 1 }

sub christian_era { $_[0]->ce_year > 0 ? 'AD' : 'BC' }
sub secular_era   { $_[0]->ce_year > 0 ? 'CE' : 'BCE' }

sub year_with_era { (abs $_[0]->ce_year) . $_[0]->era_abbr }
sub year_with_christian_era { (abs $_[0]->ce_year) . $_[0]->christian_era }
sub year_with_secular_era   { (abs $_[0]->ce_year) . $_[0]->secular_era }

sub month   {
    Carp::carp('month() is a read-only accessor') if @_ > 1;
    return $_[0]->{local_c}{month};
}
*mon = \&month;

sub month_0 { $_[0]->{local_c}{month} - 1 }
*mon_0 = \&month_0;

sub month_name { $_[0]->{locale}->month_format_wide->[ $_[0]->month_0() ] }

sub month_abbr { $_[0]->{locale}->month_format_abbreviated->[ $_[0]->month_0() ] }

sub day_of_month {
    Carp::carp('day_of_month() is a read-only accessor') if @_ > 1;
    $_[0]->{local_c}{day};
}
*day  = \&day_of_month;
*mday = \&day_of_month;

sub weekday_of_month { use integer; ( ( $_[0]->day - 1 ) / 7 ) + 1 }

sub quarter { $_[0]->{local_c}{quarter} }

sub quarter_name { $_[0]->{locale}->quarter_format_wide->[ $_[0]->quarter_0() ] }
sub quarter_abbr { $_[0]->{locale}->quarter_format_abbreviated->[ $_[0]->quarter_0() ] }

sub quarter_0 { $_[0]->{local_c}{quarter} - 1 }

sub day_of_month_0 { $_[0]->{local_c}{day} - 1 }
*day_0  = \&day_of_month_0;
*mday_0 = \&day_of_month_0;

sub day_of_week { $_[0]->{local_c}{day_of_week} }
*wday = \&day_of_week;
*dow  = \&day_of_week;

sub day_of_week_0 { $_[0]->{local_c}{day_of_week} - 1 }
*wday_0 = \&day_of_week_0;
*dow_0  = \&day_of_week_0;

sub local_day_of_week
{
    my $self = shift;

    my $day = $self->day_of_week();

    my $local_first_day = $self->{locale}->first_day_of_week();

    my $d = ( ( 8 - $local_first_day ) + $day ) % 7;

    return $d == 0 ? 7 : $d;
}

sub day_name { $_[0]->{locale}->day_format_wide->[ $_[0]->day_of_week_0() ] }

sub day_abbr { $_[0]->{locale}->day_format_abbreviated->[ $_[0]->day_of_week_0() ] }

sub day_of_quarter { $_[0]->{local_c}{day_of_quarter} }
*doq = \&day_of_quarter;

sub day_of_quarter_0 { $_[0]->day_of_quarter - 1 }
*doq_0 = \&day_of_quarter_0;

sub day_of_year { $_[0]->{local_c}{day_of_year} }
*doy = \&day_of_year;

sub day_of_year_0 { $_[0]->{local_c}{day_of_year} - 1 }
*doy_0 = \&day_of_year_0;

sub am_or_pm { $_[0]->{locale}->am_pm_abbreviated->[ $_[0]->hour() < 12 ? 0 : 1 ] }

sub ymd
{
    my ( $self, $sep ) = @_;
    $sep = '-' unless defined $sep;

    return sprintf( "%0.4d%s%0.2d%s%0.2d",
                    $self->year, $sep,
                    $self->{local_c}{month}, $sep,
                    $self->{local_c}{day} );
}
*date = \&ymd;

sub mdy
{
    my ( $self, $sep ) = @_;
    $sep = '-' unless defined $sep;

    return sprintf( "%0.2d%s%0.2d%s%0.4d",
                    $self->{local_c}{month}, $sep,
                    $self->{local_c}{day}, $sep,
                    $self->year );
}

sub dmy
{
    my ( $self, $sep ) = @_;
    $sep = '-' unless defined $sep;

    return sprintf( "%0.2d%s%0.2d%s%0.4d",
                    $self->{local_c}{day}, $sep,
                    $self->{local_c}{month}, $sep,
                    $self->year );
}

sub hour   {
    Carp::carp('hour() is a read-only accessor') if @_ > 1;
    return $_[0]->{local_c}{hour};
}
sub hour_1 { $_[0]->{local_c}{hour} == 0 ? 24 : $_[0]->{local_c}{hour} }

sub hour_12   { my $h = $_[0]->hour % 12; return $h ? $h : 12 }
sub hour_12_0 { $_[0]->hour % 12 }

sub minute {
    Carp::carp('minute() is a read-only accessor') if @_ > 1;
    return $_[0]->{local_c}{minute};
}
*min = \&minute;

sub second {
    Carp::carp('second() is a read-only accessor') if @_ > 1;
    return $_[0]->{local_c}{second};
}
*sec = \&second;

sub fractional_second { $_[0]->second + $_[0]->nanosecond / MAX_NANOSECONDS }

sub nanosecond {
    Carp::carp('nanosecond() is a read-only accessor') if @_ > 1;
    return $_[0]->{rd_nanosecs};
}

sub millisecond { _round( $_[0]->{rd_nanosecs} / 1000000 ) }

sub microsecond { _round( $_[0]->{rd_nanosecs} / 1000 ) }

sub _round
{
    my $val = shift;
    my $int = int $val;

    return $val - $int >= 0.5 ? $int + 1 : $int;
}

sub leap_seconds
{
    my $self = shift;

    return 0 if $self->{tz}->is_floating;

    return DateTime->_accumulated_leap_seconds( $self->{utc_rd_days} );
}

sub _stringify
{
    my $self = shift;

    return $self->iso8601 unless $self->{formatter};
    return $self->{formatter}->format_datetime($self);
}

sub hms
{
    my ( $self, $sep ) = @_;
    $sep = ':' unless defined $sep;

    return sprintf( "%0.2d%s%0.2d%s%0.2d",
                    $self->{local_c}{hour}, $sep,
                    $self->{local_c}{minute}, $sep,
                    $self->{local_c}{second} );
}
# don't want to override CORE::time()
*DateTime::time = \&hms;

sub iso8601 { join 'T', $_[0]->ymd('-'), $_[0]->hms(':') }
*datetime = \&iso8601;

sub is_leap_year { $_[0]->_is_leap_year( $_[0]->year ) }

sub week
{
    my $self = shift;

    unless ( defined $self->{local_c}{week_year} )
    {
        # This algorithm was taken from Date::Calc's DateCalc.c file
        my $jan_one_dow_m1 =
            ( ( $self->_ymd2rd( $self->year, 1, 1 ) + 6 ) % 7 );

        $self->{local_c}{week_number} =
            int( ( ( $self->day_of_year - 1 ) + $jan_one_dow_m1 ) / 7 );
        $self->{local_c}{week_number}++ if $jan_one_dow_m1 < 4;

        if ( $self->{local_c}{week_number} == 0 )
        {
            $self->{local_c}{week_year} = $self->year - 1;
            $self->{local_c}{week_number} =
                $self->_weeks_in_year( $self->{local_c}{week_year} );
        }
        elsif ( $self->{local_c}{week_number} == 53 &&
                $self->_weeks_in_year( $self->year ) == 52 )
        {
            $self->{local_c}{week_number} = 1;
            $self->{local_c}{week_year} = $self->year + 1;
        }
        else
        {
            $self->{local_c}{week_year} = $self->year;
        }
    }

    return @{ $self->{local_c} }{ 'week_year', 'week_number' }
}

# Also from DateCalc.c
sub _weeks_in_year
{
    my $self = shift;
    my $year = shift;

    my $jan_one_dow =
        ( ( $self->_ymd2rd( $year, 1, 1 ) + 6 ) % 7 ) + 1;
    my $dec_31_dow =
        ( ( $self->_ymd2rd( $year, 12, 31 ) + 6 ) % 7 ) + 1;

    return $jan_one_dow == 4 || $dec_31_dow == 4 ? 53 : 52;
}

sub week_year   { ($_[0]->week)[0] }
sub week_number { ($_[0]->week)[1] }

# ISO says that the first week of a year is the first week containing
# a Thursday.  Extending that says that the first week of the month is
# the first week containing a Thursday.  ICU agrees.
#
# Algorithm supplied by Rick Measham, who doesn't understand how it
# works.  Neither do I.  Please feel free to explain this to me!
sub week_of_month
{
    my $self = shift;

    # Faster than cloning just to get the dow
    my $first_wday_of_month = ( 8 - ( $self->day - $self->dow ) % 7 ) % 7;
    $first_wday_of_month = 7 unless $first_wday_of_month;

    my $wom = int( ( $self->day + $first_wday_of_month - 2 ) / 7 );
    return ( $first_wday_of_month <= 4 ) ? $wom + 1 : $wom;
}

sub time_zone {
    Carp::carp('time_zone() is a read-only accessor') if @_ > 1;
    return $_[0]->{tz};
}

sub offset                     { $_[0]->{tz}->offset_for_datetime( $_[0] ) }
sub _offset_for_local_datetime { $_[0]->{tz}->offset_for_local_datetime( $_[0] ) }

sub is_dst { $_[0]->{tz}->is_dst_for_datetime( $_[0] ) }

sub time_zone_long_name  { $_[0]->{tz}->name }
sub time_zone_short_name { $_[0]->{tz}->short_name_for_datetime( $_[0] ) }

sub locale {
    Carp::carp('locale() is a read-only accessor') if @_ > 1;
    return $_[0]->{locale};
}
*language = \&locale;

sub utc_rd_values { @{ $_[0] }{ 'utc_rd_days', 'utc_rd_secs', 'rd_nanosecs' } }
sub local_rd_values { @{ $_[0] }{ 'local_rd_days', 'local_rd_secs', 'rd_nanosecs' } }

# NOTE: no nanoseconds, no leap seconds
sub utc_rd_as_seconds   { ( $_[0]->{utc_rd_days} * SECONDS_PER_DAY ) + $_[0]->{utc_rd_secs} }

# NOTE: no nanoseconds, no leap seconds
sub local_rd_as_seconds { ( $_[0]->{local_rd_days} * SECONDS_PER_DAY ) + $_[0]->{local_rd_secs} }

# RD 1 is JD 1,721,424.5 - a simple offset
sub jd
{
    my $self = shift;

    my $jd = $self->{utc_rd_days} + 1_721_424.5;

    my $day_length = $self->_day_length( $self->{utc_rd_days} );

    return ( $jd +
             ( $self->{utc_rd_secs} / $day_length )  +
             ( $self->{rd_nanosecs} / $day_length / MAX_NANOSECONDS )
           );
}

sub mjd { $_[0]->jd - 2_400_000.5 }

{
    my %strftime_patterns =
        ( 'a' => sub { $_[0]->day_abbr },
          'A' => sub { $_[0]->day_name },
          'b' => sub { $_[0]->month_abbr },
          'B' => sub { $_[0]->month_name },
          'c' => sub { $_[0]->format_cldr( $_[0]->{locale}->datetime_format_default() ) },
          'C' => sub { int( $_[0]->year / 100 ) },
          'd' => sub { sprintf( '%02d', $_[0]->day_of_month ) },
          'D' => sub { $_[0]->strftime( '%m/%d/%y' ) },
          'e' => sub { sprintf( '%2d', $_[0]->day_of_month ) },
          'F' => sub { $_[0]->ymd('-') },
          'g' => sub { substr( $_[0]->week_year, -2 ) },
          'G' => sub { $_[0]->week_year },
          'H' => sub { sprintf( '%02d', $_[0]->hour ) },
          'I' => sub { sprintf( '%02d', $_[0]->hour_12 ) },
          'j' => sub { $_[0]->day_of_year },
          'k' => sub { sprintf( '%2d', $_[0]->hour ) },
          'l' => sub { sprintf( '%2d', $_[0]->hour_12 ) },
          'm' => sub { sprintf( '%02d', $_[0]->month ) },
          'M' => sub { sprintf( '%02d', $_[0]->minute ) },
          'n' => sub { "\n" }, # should this be OS-sensitive?
          'N' => \&_format_nanosecs,
          'p' => sub { $_[0]->am_or_pm() },
          'P' => sub { lc $_[0]->am_or_pm() },
          'r' => sub { $_[0]->strftime( '%I:%M:%S %p' ) },
          'R' => sub { $_[0]->strftime( '%H:%M' ) },
          's' => sub { $_[0]->epoch },
          'S' => sub { sprintf( '%02d', $_[0]->second ) },
          't' => sub { "\t" },
          'T' => sub { $_[0]->strftime( '%H:%M:%S' ) },
          'u' => sub { $_[0]->day_of_week },
          # algorithm from Date::Format::wkyr
          'U' => sub { my $dow = $_[0]->day_of_week;
                       $dow = 0 if $dow == 7; # convert to 0-6, Sun-Sat
                       my $doy = $_[0]->day_of_year - 1;
                       return sprintf( '%02d', int( ( $doy - $dow + 13 ) / 7 - 1 ) )
                   },
          'V' => sub { sprintf( '%02d', $_[0]->week_number ) },
          'w' => sub { my $dow = $_[0]->day_of_week;
                       return $dow % 7;
                   },
          'W' => sub { my $dow = $_[0]->day_of_week;
                       my $doy = $_[0]->day_of_year - 1;
                       return sprintf( '%02d', int( ( $doy - $dow + 13 ) / 7 - 1 ) )
                   },
          'x' => sub { $_[0]->format_cldr( $_[0]->{locale}->date_format_default() ) },
          'X' => sub { $_[0]->format_cldr( $_[0]->{locale}->time_format_default() ) },
          'y' => sub { sprintf( '%02d', substr( $_[0]->year, -2 ) ) },
          'Y' => sub { return $_[0]->year },
          'z' => sub { DateTime::TimeZone->offset_as_string( $_[0]->offset ) },
          'Z' => sub { $_[0]->{tz}->short_name_for_datetime( $_[0] ) },
          '%' => sub { '%' },
        );

    $strftime_patterns{h} = $strftime_patterns{b};

    sub strftime
    {
        my $self = shift;
        # make a copy or caller's scalars get munged
        my @patterns = @_;

        my @r;
        foreach my $p (@patterns)
        {
            $p =~ s/
                    (?:
                      %{(\w+)}         # method name like %{day_name}
                      |
                      %([%a-zA-Z])     # single character specifier like %d
                      |
                      %(\d+)N          # special case for %N
                    )
                   /
                    ( $1
                      ? ( $self->can($1) ? $self->$1() : "\%{$1}" )
                      : $2
                      ? ( $strftime_patterns{$2} ? $strftime_patterns{$2}->($self) : "\%$2" )
                      : $3
                      ? $strftime_patterns{N}->($self, $3)
                      : ''  # this won't happen
                    )
                   /sgex;

            return $p unless wantarray;

            push @r, $p;
        }

        return @r;
    }
}

{
    # It's an array because the order in which the regexes are checked
    # is important. These patterns are similar to the ones Java uses,
    # but not quite the same. See
    # http://www.unicode.org/reports/tr35/tr35-9.html#Date_Format_Patterns.
    my @patterns =
        ( qr/GGGGG/  => sub { $_[0]->{locale}->era_narrow->[ $_[0]->_era_index() ] },
          qr/GGGG/   => 'era_name',
          qr/G{1,3}/ => 'era_abbr',

          qr/(y{3,5})/ => sub { $_[0]->_zero_padded_number( $1, $_[0]->year() ) },
          # yy is a weird special case, where it must be exactly 2 digits
          qr/yy/       => sub { my $year = $_[0]->year();
                                my $y2 = substr( $year, -2, 2 ) if length $year > 2;
                                $y2 *= -1 if $year < 0;
                                $_[0]->_zero_padded_number( 'yy', $y2 ) },
          qr/y/        => sub { $_[0]->year() },
          qr/(u+)/     => sub { $_[0]->_zero_padded_number( $1, $_[0]->year() ) },
          qr/(Y+)/     => sub { $_[0]->_zero_padded_number( $1, $_[0]->week_year() ) },

          qr/QQQQ/  => 'quarter_name',
          qr/QQQ/   => 'quarter_abbr',
          qr/(QQ?)/ => sub { $_[0]->_zero_padded_number( $1, $_[0]->quarter() ) },

          qr/qqqq/  => sub { $_[0]->{locale}->quarter_stand_alone_wide()->[ $_[0]->quarter_0() ] },
          qr/qqq/   => sub { $_[0]->{locale}->quarter_stand_alone_abbreviated()->[ $_[0]->quarter_0() ] },
          qr/(qq?)/ => sub { $_[0]->_zero_padded_number( $1, $_[0]->quarter() ) },

          qr/MMMMM/ => sub { $_[0]->{locale}->month_format_narrow->[ $_[0]->month_0() ] },
          qr/MMMM/  => 'month_name',
          qr/MMM/   => 'month_abbr',
          qr/(MM?)/ => sub { $_[0]->_zero_padded_number( $1, $_[0]->month() ) },

          qr/LLLLL/ => sub { $_[0]->{locale}->month_stand_alone_narrow->[ $_[0]->month_0() ] },
          qr/LLLL/  => sub { $_[0]->{locale}->month_stand_alone_wide->[ $_[0]->month_0() ] },
          qr/LLL/   => sub { $_[0]->{locale}->month_stand_alone_abbreviated->[ $_[0]->month_0() ] },
          qr/(LL?)/ => sub { $_[0]->_zero_padded_number( $1, $_[0]->month() ) },

          qr/(ww?)/ => sub { $_[0]->_zero_padded_number( $1, $_[0]->week_number() ) },
          qr/W/     => 'week_of_month',

          qr/(dd?)/    => sub { $_[0]->_zero_padded_number( $1, $_[0]->day_of_month() ) },
          qr/(D{1,3})/ => sub { $_[0]->_zero_padded_number( $1, $_[0]->day_of_year() ) },

          qr/F/    => 'weekday_of_month',
          qr/(g+)/ => sub { $_[0]->_zero_padded_number( $1, $_[0]->mjd() ) },

          qr/EEEEE/  => sub { $_[0]->{locale}->day_format_narrow->[ $_[0]->day_of_week_0() ] },
          qr/EEEE/   => 'day_name',
          qr/E{1,3}/ => 'day_abbr',

          qr/eeeee/ => sub { $_[0]->{locale}->day_format_narrow->[ $_[0]->day_of_week_0() ] },
          qr/eeee/  => 'day_name',
          qr/eee/   => 'day_abbr',
          qr/(ee?)/ => sub { $_[0]->_zero_padded_number( $1, $_[0]->local_day_of_week() ) },

          qr/ccccc/ => sub { $_[0]->{locale}->day_stand_alone_narrow->[ $_[0]->day_of_week_0() ] },
          qr/cccc/  => sub { $_[0]->{locale}->day_stand_alone_wide->[ $_[0]->day_of_week_0() ] },
          qr/ccc/   => sub { $_[0]->{locale}->day_stand_alone_abbreviated->[ $_[0]->day_of_week_0() ] },
          qr/(cc?)/ => sub { $_[0]->_zero_padded_number( $1, $_[0]->day_of_week() ) },

          qr/a/ => 'am_or_pm',

          qr/(hh?)/ => sub { $_[0]->_zero_padded_number( $1, $_[0]->hour_12() ) },
          qr/(HH?)/ => sub { $_[0]->_zero_padded_number( $1, $_[0]->hour() ) },
          qr/(KK?)/ => sub { $_[0]->_zero_padded_number( $1, $_[0]->hour_12_0() ) },
          qr/(kk?)/ => sub { $_[0]->_zero_padded_number( $1, $_[0]->hour_1() ) },
          qr/(jj?)/ => sub { my $h = $_[0]->{locale}->prefers_24_hour_time() ? $_[0]->hour() : $_[0]->hour_12();
                             $_[0]->_zero_padded_number( $1, $h ) },

          qr/(mm?)/ => sub { $_[0]->_zero_padded_number( $1, $_[0]->minute() ) },

          qr/(ss?)/ => sub { $_[0]->_zero_padded_number( $1, $_[0]->second() ) },
          # I'm not sure this is what is wanted (notably the trailing
          # and leading zeros it can produce), but once again the LDML
          # spec is not all that clear.
          qr/(S+)/  => sub { my $l = length $1;
                             my $val = sprintf( "%.${l}f", $_[0]->fractional_second() - $_[0]->second() );
                             $val =~ s/^0\.//;
                             $val || 0 },
          qr/A+/    => sub { ( $_[0]->{local_rd_secs} * 1000 ) + $_[0]->millisecond() },

          qr/zzzz/   => sub { $_[0]->time_zone_long_name() },
          qr/z{1,3}/ => sub { $_[0]->time_zone_short_name() },
          qr/ZZZZ/   => sub { $_[0]->time_zone_short_name()
                              . DateTime::TimeZone->offset_as_string( $_[0]->offset() ) },
          qr/Z{1,3}/ => sub { DateTime::TimeZone->offset_as_string( $_[0]->offset() ) },
          qr/vvvv/   => sub { $_[0]->time_zone_long_name() },
          qr/v{1,3}/ => sub { $_[0]->time_zone_short_name() },
          qr/VVVV/   => sub { $_[0]->time_zone_long_name() },
          qr/V{1,3}/ => sub { $_[0]->time_zone_short_name() },
    );

    sub _zero_padded_number
    {
        my $self = shift;
        my $size = length shift;
        my $val  = shift;

        return sprintf( "%0${size}d", $val );
    }

    sub _space_padded_string
    {
        my $self = shift;
        my $size = length shift;
        my $val  = shift;

        return sprintf( "% ${size}s", $val );
    }

    sub format_cldr
    {
        my $self = shift;
        # make a copy or caller's scalars get munged
        my @patterns = @_;

        my @r;
        foreach my $p (@patterns)
        {
            $p =~ s/\G
                    (?:
                      '((?:[^']|'')*)' # quote escaped bit of text
                                       # it needs to end with one
                                       # quote not followed by
                                       # another
                      |
                      (([a-zA-Z])\3*)     # could be a pattern
                      |
                      (.)                 # anything else
                    )
                   /
                    defined $1
                    ? $1
                    : defined $2
                    ? $self->_cldr_pattern($2)
                    : defined $4
                    ? $4
                    : undef # should never get here
                   /sgex;

            $p =~ s/\'\'/\'/g;

            return $p unless wantarray;

            push @r, $p;
        }

        return @r;
    }

    sub _cldr_pattern
    {
        my $self    = shift;
        my $pattern = shift;

        for ( my $i = 0; $i < @patterns; $i +=2 )
        {
            if ( $pattern =~ /$patterns[$i]/ )
            {
                my $sub = $patterns[ $i + 1 ];

                return $self->$sub();
            }
        }

        return $pattern;
    }
}

sub _format_nanosecs
{
    my $self = shift;
    my $precision = shift;

    my $ret = sprintf( "%09d", $self->{rd_nanosecs} );
    return $ret unless $precision;   # default = 9 digits

    # rd_nanosecs might contain a fractional separator
    my ( $int, $frac ) = split /[.,]/, $self->{rd_nanosecs};
    $ret .= $frac if $frac;

    return substr( $ret, 0, $precision );
}

sub epoch
{
    my $self = shift;

    return $self->{utc_c}{epoch}
        if exists $self->{utc_c}{epoch};

    my ( $year, $month, $day ) = $self->_utc_ymd;
    my @hms = $self->_utc_hms;

    $self->{utc_c}{epoch} =
        timegm_nocheck( ( reverse @hms ),
                        $day,
                        $month - 1,
                        $year,
                      );

    return $self->{utc_c}{epoch};
}

sub hires_epoch
{
    my $self = shift;

    my $epoch = $self->epoch;

    return undef unless defined $epoch;

    my $nano = $self->{rd_nanosecs} / MAX_NANOSECONDS;

    return $epoch + $nano;
}

sub is_finite { 1 }
sub is_infinite { 0 }

# added for benefit of DateTime::TimeZone
sub utc_year { $_[0]->{utc_year} }

# returns a result that is relative to the first datetime
sub subtract_datetime
{
    my $dt1 = shift;
    my $dt2 = shift;

    $dt2 = $dt2->clone->set_time_zone( $dt1->time_zone )
        unless $dt1->time_zone eq $dt2->time_zone;

    # We only want a negative duration if $dt2 > $dt1 ($self)
    my ( $bigger, $smaller, $negative ) =
        ( $dt1 >= $dt2 ?
          ( $dt1, $dt2, 0 ) :
          ( $dt2, $dt1, 1 )
        );

    my $is_floating = $dt1->time_zone->is_floating &&
                      $dt2->time_zone->is_floating;


    my $minute_length = 60;
    unless ($is_floating)
    {
        my ( $utc_rd_days, $utc_rd_secs ) = $smaller->utc_rd_values;

        if ( $utc_rd_secs >= 86340 && ! $is_floating )
        {
            # If the smaller of the two datetimes occurs in the last
            # UTC minute of the UTC day, then that minute may not be
            # 60 seconds long.  If we need to subtract a minute from
            # the larger datetime's minutes count in order to adjust
            # the seconds difference to be positive, we need to know
            # how long that minute was.  If one of the datetimes is
            # floating, we just assume a minute is 60 seconds.

            $minute_length = $dt1->_day_length($utc_rd_days) - 86340;
        }
    }

    # This is a gross hack that basically figures out if the bigger of
    # the two datetimes is the day of a DST change.  If it's a 23 hour
    # day (switching _to_ DST) then we subtract 60 minutes from the
    # local time.  If it's a 25 hour day then we add 60 minutes to the
    # local time.
    #
    # This produces the most "intuitive" results, though there are
    # still reversibility problems with the resultant duration.
    #
    # However, if the two objects are on the same (local) date, and we
    # are not crossing a DST change, we don't want to invoke the hack
    # - see 38local-subtract.t
    my $bigger_min = $bigger->hour * 60 + $bigger->minute;
    if ( $bigger->time_zone->has_dst_changes
         && $bigger->is_dst != $smaller->is_dst
       )
    {

        $bigger_min -= 60
            # it's a 23 hour (local) day
            if ( $bigger->is_dst
                 &&
                 do { local $@;
                      my $prev_day = eval { $bigger->clone->subtract( days => 1 ) };
                      $prev_day && ! $prev_day->is_dst ? 1 : 0 }
               );

        $bigger_min += 60
            # it's a 25 hour (local) day
            if ( ! $bigger->is_dst
                 &&
                 do { local $@;
                      my $prev_day = eval { $bigger->clone->subtract( days => 1 ) };
                      $prev_day && $prev_day->is_dst ? 1 : 0 }
               );
    }

    my ( $months, $days, $minutes, $seconds, $nanoseconds ) =
        $dt1->_adjust_for_positive_difference
            ( $bigger->year * 12 + $bigger->month, $smaller->year * 12 + $smaller->month,

              $bigger->day, $smaller->day,

              $bigger_min, $smaller->hour * 60 + $smaller->minute,

	      $bigger->second, $smaller->second,

	      $bigger->nanosecond, $smaller->nanosecond,

	      $minute_length,

              # XXX - using the smaller as the month length is
              # somewhat arbitrary, we could also use the bigger -
              # either way we have reversibility problems
	      $dt1->_month_length( $smaller->year, $smaller->month ),
            );

    if ($negative)
    {
        for ( $months, $days, $minutes, $seconds, $nanoseconds )
        {
	    # Some versions of Perl can end up with -0 if we do "0 * -1"!!
            $_ *= -1 if $_;
        }
    }

    return
        $dt1->duration_class->new
            ( months      => $months,
	      days        => $days,
	      minutes     => $minutes,
              seconds     => $seconds,
              nanoseconds => $nanoseconds,
            );
}

sub _adjust_for_positive_difference
{
    my ( $self,
	 $month1, $month2,
	 $day1, $day2,
	 $min1, $min2,
	 $sec1, $sec2,
	 $nano1, $nano2,
	 $minute_length,
	 $month_length,
       ) = @_;

    if ( $nano1 < $nano2 )
    {
        $sec1--;
        $nano1 += MAX_NANOSECONDS;
    }

    if ( $sec1 < $sec2 )
    {
        $min1--;
        $sec1 += $minute_length;
    }

    # A day always has 24 * 60 minutes, though the minutes may vary in
    # length.
    if ( $min1 < $min2 )
    {
	$day1--;
	$min1 += 24 * 60;
    }

    if ( $day1 < $day2 )
    {
	$month1--;
	$day1 += $month_length;
    }

    return ( $month1 - $month2,
	     $day1 - $day2,
	     $min1 - $min2,
             $sec1 - $sec2,
             $nano1 - $nano2,
           );
}

sub subtract_datetime_absolute
{
    my $self = shift;
    my $dt = shift;

    my $utc_rd_secs1 = $self->utc_rd_as_seconds;
    $utc_rd_secs1 += DateTime->_accumulated_leap_seconds( $self->{utc_rd_days} )
	if ! $self->time_zone->is_floating;

    my $utc_rd_secs2 = $dt->utc_rd_as_seconds;
    $utc_rd_secs2 += DateTime->_accumulated_leap_seconds( $dt->{utc_rd_days} )
	if ! $dt->time_zone->is_floating;

    my $seconds = $utc_rd_secs1 - $utc_rd_secs2;
    my $nanoseconds = $self->nanosecond - $dt->nanosecond;

    if ( $nanoseconds < 0 )
    {
	$seconds--;
	$nanoseconds += MAX_NANOSECONDS;
    }

    return
        $self->duration_class->new
            ( seconds     => $seconds,
              nanoseconds => $nanoseconds,
            );
}

sub delta_md
{
    my $self = shift;
    my $dt = shift;

    my ( $smaller, $bigger ) = sort $self, $dt;

    my ( $months, $days, undef, undef, undef ) =
        $dt->_adjust_for_positive_difference
            ( $bigger->year * 12 + $bigger->month, $smaller->year * 12 + $smaller->month,

              $bigger->day, $smaller->day,

              0, 0,

              0, 0,

              0, 0,

	      60,

	      $smaller->_month_length( $smaller->year, $smaller->month ),
            );

    return $self->duration_class->new( months => $months,
                                       days   => $days );
}

sub delta_days
{
    my $self = shift;
    my $dt = shift;

    my $days = abs( ($self->local_rd_values)[0] - ($dt->local_rd_values)[0] );

    $self->duration_class->new( days => $days );
}

sub delta_ms
{
    my $self = shift;
    my $dt = shift;

    my ( $smaller, $greater ) = sort $self, $dt;

    my $days = int( $greater->jd - $smaller->jd );

    my $dur = $greater->subtract_datetime($smaller);

    my %p;
    $p{hours}   = $dur->hours + ( $days * 24 );
    $p{minutes} = $dur->minutes;
    $p{seconds} = $dur->seconds;

    return $self->duration_class->new(%p);
}

sub _add_overload
{
    my ( $dt, $dur, $reversed ) = @_;

    if ($reversed)
    {
        ( $dur, $dt ) = ( $dt, $dur );
    }

    unless ( DateTime::Helpers::isa( $dur, 'DateTime::Duration' ) )
    {
        my $class = ref $dt;
        my $dt_string = overload::StrVal($dt);

        Carp::croak( "Cannot add $dur to a $class object ($dt_string).\n"
                     . " Only a DateTime::Duration object can "
                     . " be added to a $class object." );
    }

    return $dt->clone->add_duration($dur);
}

sub _subtract_overload
{
    my ( $date1, $date2, $reversed ) = @_;

    if ($reversed)
    {
        ( $date2, $date1 ) = ( $date1, $date2 );
    }

    if ( DateTime::Helpers::isa( $date2, 'DateTime::Duration' ) )
    {
        my $new = $date1->clone;
        $new->add_duration( $date2->inverse );
        return $new;
    }
    elsif ( DateTime::Helpers::isa( $date2, 'DateTime' ) )
    {
        return $date1->subtract_datetime($date2);
    }
    else
    {
        my $class = ref $date1;
        my $dt_string = overload::StrVal($date1);

        Carp::croak( "Cannot subtract $date2 from a $class object ($dt_string).\n"
                     . " Only a DateTime::Duration or DateTime object can "
                     . " be subtracted from a $class object." );
    }
}

sub add
{
    my $self = shift;

    return $self->add_duration( $self->duration_class->new(@_) );
}

sub subtract
{
    my $self = shift;

    return $self->subtract_duration( $self->duration_class->new(@_) );
}

sub subtract_duration { return $_[0]->add_duration( $_[1]->inverse ) }

{
    my @spec = ( { isa => 'DateTime::Duration' } );
    sub add_duration
    {
        my $self = shift;
        my ($dur) = validate_pos( @_, @spec );

        # simple optimization
        return $self if $dur->is_zero;

        my %deltas = $dur->deltas;

        # This bit isn't quite right since DateTime::Infinite::Future -
        # infinite duration should NaN
        foreach my $val ( values %deltas )
        {
            my $inf;
            if ( $val == INFINITY )
            {
                $inf = DateTime::Infinite::Future->new;
            }
            elsif ( $val == NEG_INFINITY )
            {
                $inf = DateTime::Infinite::Past->new;
            }

            if ($inf)
            {
                %$self = %$inf;
                bless $self, ref $inf;

                return $self;
            }
        }

        return $self if $self->is_infinite;

        if ( $deltas{days} )
        {
            $self->{local_rd_days} += $deltas{days};

            $self->{utc_year} += int( $deltas{days} / 365 ) + 1;
        }

        if ( $deltas{months} )
        {
            # For preserve mode, if it is the last day of the month, make
            # it the 0th day of the following month (which then will
            # normalize back to the last day of the new month).
            my ($y, $m, $d) = ( $dur->is_preserve_mode ?
                                $self->_rd2ymd( $self->{local_rd_days} + 1 ) :
                                $self->_rd2ymd( $self->{local_rd_days} )
                              );

            $d -= 1 if $dur->is_preserve_mode;

            if ( ! $dur->is_wrap_mode && $d > 28 )
            {
                # find the rd for the last day of our target month
                $self->{local_rd_days} = $self->_ymd2rd( $y, $m + $deltas{months} + 1, 0 );

                # what day of the month is it? (discard year and month)
                my $last_day = ($self->_rd2ymd( $self->{local_rd_days} ))[2];

                # if our original day was less than the last day,
                # use that instead
                $self->{local_rd_days} -= $last_day - $d if $last_day > $d;
            }
            else
            {
                $self->{local_rd_days} = $self->_ymd2rd( $y, $m + $deltas{months}, $d );
            }

            $self->{utc_year} += int( $deltas{months} / 12 ) + 1;
        }

        if ( $deltas{days} || $deltas{months} )
        {
            $self->_calc_utc_rd;

            $self->_handle_offset_modifier( $self->second );
        }

        if ( $deltas{minutes} )
        {
            $self->{utc_rd_secs} += $deltas{minutes} * 60;

            # This intentionally ignores leap seconds
            $self->_normalize_tai_seconds( $self->{utc_rd_days}, $self->{utc_rd_secs} );
        }

        if ( $deltas{seconds} || $deltas{nanoseconds} )
        {
            $self->{utc_rd_secs} += $deltas{seconds};

            if ( $deltas{nanoseconds} )
            {
                $self->{rd_nanosecs} += $deltas{nanoseconds};
                $self->_normalize_nanoseconds( $self->{utc_rd_secs}, $self->{rd_nanosecs} );
            }

            $self->_normalize_seconds;

            # This might be some big number much bigger than 60, but
            # that's ok (there are tests in 19leap_second.t to confirm
            # that)
            $self->_handle_offset_modifier( $self->second + $deltas{seconds} );
        }

        my $new =
            (ref $self)->from_object
                ( object => $self,
                  locale => $self->{locale},
                  ( $self->{formatter} ? ( formatter => $self->{formatter} ) : () ),
                 );

        %$self = %$new;

        return $self;
    }
}

sub _compare_overload
{
    # note: $_[1]->compare( $_[0] ) is an error when $_[1] is not a
    # DateTime (such as the INFINITY value)
    return $_[2] ? - $_[0]->compare( $_[1] ) : $_[0]->compare( $_[1] );
}

sub compare
{
    shift->_compare( @_, 0 );
}

sub compare_ignore_floating
{
    shift->_compare( @_, 1 );
}

sub _compare
{
    my ( $class, $dt1, $dt2, $consistent ) = ref $_[0] ? ( undef, @_ ) : @_;

    return undef unless defined $dt2;

    if ( ! ref $dt2 && ( $dt2 == INFINITY || $dt2 == NEG_INFINITY ) )
    {
        return $dt1->{utc_rd_days} <=> $dt2;
    }

    unless ( DateTime::Helpers::can( $dt1, 'utc_rd_values' )
             && DateTime::Helpers::can( $dt2, 'utc_rd_values' ) )
    {
        my $dt1_string = overload::StrVal($dt1);
        my $dt2_string = overload::StrVal($dt2);

        Carp::croak( "A DateTime object can only be compared to"
                     . " another DateTime object ($dt1_string, $dt2_string)." );
    }

    if ( ! $consistent &&
         DateTime::Helpers::can( $dt1, 'time_zone' ) &&
         DateTime::Helpers::can( $dt2, 'time_zone' )
       )
    {
        my $is_floating1 = $dt1->time_zone->is_floating;
        my $is_floating2 = $dt2->time_zone->is_floating;

        if ( $is_floating1 && ! $is_floating2 )
        {
            $dt1 = $dt1->clone->set_time_zone( $dt2->time_zone );
        }
        elsif ( $is_floating2 && ! $is_floating1 )
        {
            $dt2 = $dt2->clone->set_time_zone( $dt1->time_zone );
        }
    }

    my @dt1_components = $dt1->utc_rd_values;
    my @dt2_components = $dt2->utc_rd_values;

    foreach my $i ( 0..2 )
    {
        return $dt1_components[$i] <=> $dt2_components[$i]
            if $dt1_components[$i] != $dt2_components[$i]
    }

    return 0;
}

sub _string_equals_overload
{
    my ( $class, $dt1, $dt2 ) = ref $_[0] ? ( undef, @_ ) : @_;

    return unless
        (    DateTime::Helpers::can( $dt1, 'utc_rd_values' )
          && DateTime::Helpers::can( $dt2, 'utc_rd_values' )
        );

    $class ||= ref $dt1;
    return ! $class->compare( $dt1, $dt2 );
}

sub _string_not_equals_overload
{
    return ! _string_equals_overload(@_);
}

sub _normalize_nanoseconds
{
    use integer;

    # seconds, nanoseconds
    if ( $_[2] < 0 )
    {
        my $overflow = 1 + $_[2] / MAX_NANOSECONDS;
        $_[2] += $overflow * MAX_NANOSECONDS;
        $_[1] -= $overflow;
    }
    elsif ( $_[2] >= MAX_NANOSECONDS )
    {
        my $overflow = $_[2] / MAX_NANOSECONDS;
        $_[2] -= $overflow * MAX_NANOSECONDS;
        $_[1] += $overflow;
    }
}

# Many of the same parameters as new() but all of them are optional,
# and there are no defaults.
my $SetValidate =
    { map { my %copy = %{ $BasicValidate->{$_} };
            delete $copy{default};
            $copy{optional} = 1;
            $_ => \%copy }
      keys %$BasicValidate };

sub set
{
    my $self = shift;
    my %p = validate( @_, $SetValidate );

    my $new_dt = $self->_new_from_self(%p);

    %$self = %$new_dt;

    return $self;
}

sub set_year   { $_[0]->set( year => $_[1] ) }
sub set_month  { $_[0]->set( month => $_[1] ) }
sub set_day    { $_[0]->set( day => $_[1] ) }
sub set_hour   { $_[0]->set( hour => $_[1] ) }
sub set_minute { $_[0]->set( minute => $_[1] ) }
sub set_second { $_[0]->set( second => $_[1] ) }
sub set_nanosecond { $_[0]->set( nanosecond => $_[1] ) }

sub set_locale { $_[0]->set( locale => $_[1] ) }

sub set_formatter { $_[0]->{formatter} = $_[1] }

{
    my %TruncateDefault = ( month  => 1,
                            day    => 1,
                            hour   => 0,
                            minute => 0,
                            second => 0,
                            nanosecond => 0,
                          );
    my $re = join '|', 'year', 'week', grep { $_ ne 'nanosecond' } keys %TruncateDefault;
    my $spec = { to => { regex => qr/^(?:$re)/ } };

    sub truncate
    {
        my $self = shift;
        my %p = validate( @_, $spec );

        my %new;
        if ( $p{to} eq 'week' )
        {
            my $day_diff = $self->day_of_week - 1;

            if ($day_diff)
            {
                $self->add( days => -1 * $day_diff );
            }

            return $self->truncate( to => 'day' );
        }
        else
        {
            my $truncate;
            foreach my $f ( qw( year month day hour minute second nanosecond ) )
            {
                $new{$f} = $truncate ? $TruncateDefault{$f} : $self->$f();

                $truncate = 1 if $p{to} eq $f;
            }
        }

        my $new_dt = $self->_new_from_self(%new);

        %$self = %$new_dt;

        return $self;
    }
}

sub set_time_zone
{
    my ( $self, $tz ) = @_;

    # This is a bit of a hack but it works because time zone objects
    # are singletons, and if it doesn't work all we lose is a little
    # bit of speed.
    return $self if $self->{tz} eq $tz;

    my $was_floating = $self->{tz}->is_floating;

    $self->{tz} = ref $tz ? $tz : DateTime::TimeZone->new( name => $tz );

    $self->_handle_offset_modifier( $self->second, 1 );

    # if it either was or now is floating (but not both)
    if ( $self->{tz}->is_floating xor $was_floating )
    {
        $self->_calc_utc_rd;
    }
    elsif ( ! $was_floating )
    {
        $self->_calc_local_rd;
    }

    return $self;
}

sub STORABLE_freeze
{
    my $self = shift;
    my $cloning = shift;

    my $serialized = '';
    foreach my $key ( qw( utc_rd_days
                          utc_rd_secs
                          rd_nanosecs ) )
    {
        $serialized .= "$key:$self->{$key}|";
    }

    # not used yet, but may be handy in the future.
    $serialized .= "version:$VERSION";

    # Formatter needs to be returned as a reference since it may be
    # undef or a class name, and Storable will complain if extra
    # return values aren't refs
    return $serialized, $self->{locale}, $self->{tz}, \$self->{formatter};
}

sub STORABLE_thaw
{
    my $self = shift;
    my $cloning = shift;
    my $serialized = shift;

    my %serialized = map { split /:/ } split /\|/, $serialized;

    my ( $locale, $tz, $formatter );

    # more recent code version
    if (@_)
    {
        ( $locale, $tz, $formatter ) = @_;
    }
    else
    {
        $tz = DateTime::TimeZone->new( name => delete $serialized{tz} );

        $locale =
            DateTime::Locale->load( exists $serialized{language}
                                    ? delete $serialized{language}
                                    : delete $serialized{locale}
                                  );
    }

    delete $serialized{version};

    my $object = bless { utc_vals => [ $serialized{utc_rd_days},
                                       $serialized{utc_rd_secs},
                                       $serialized{rd_nanosecs},
                                     ],
                         tz       => $tz,
                       }, 'DateTime::_Thawed';

    my %formatter = defined $$formatter ? ( formatter => $$formatter ) : ();
    my $new = (ref $self)->from_object( object => $object,
                                        locale => $locale,
                                        %formatter,
                                      );

    %$self = %$new;

    return $self;
}


package DateTime::_Thawed;

sub utc_rd_values { @{ $_[0]->{utc_vals} } }

sub time_zone { $_[0]->{tz} }


1;

__END__

#line 3954
