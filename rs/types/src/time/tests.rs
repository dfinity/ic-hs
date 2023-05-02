use crate::time::{TimeInstantiationError, NANOS_PER_MILLI};
use crate::Time;
use assert_matches::assert_matches;
use std::time::SystemTime;

mod millis {
    use super::*;
    use crate::time::GENESIS;
    use std::time::Duration;

    #[test]
    fn should_convert_genesis_to_millis() {
        let genesis_millis = GENESIS.as_millis_since_unix_epoch();
        assert_eq!(genesis_millis, 1_620_328_630_000);
    }

    #[test]
    fn should_be_zero() {
        let less_than_one_milli =
            Time::from_duration(Duration::from_millis(1) - Duration::from_nanos(1));
        assert_eq!(less_than_one_milli.as_millis_since_unix_epoch(), 0);
    }

    #[test]
    fn should_ignore_sub_millis_precision() {
        let sub_milli_offset = Duration::from_millis(1) - Duration::from_nanos(1);

        let result_in_millis = (GENESIS + sub_milli_offset).as_millis_since_unix_epoch();

        assert_eq!(result_in_millis, 1_620_328_630_000);
    }

    #[test]
    fn should_not_overflow() {
        let genesis_millis = Time::from_millis_since_unix_epoch(1_620_328_630_000);
        assert_matches!( genesis_millis, Ok(time) if time == GENESIS )
    }

    #[test]
    fn should_overflow_in_year_2554() {
        // Equals 18_446_744_073_709 ms since epoch
        // Corresponds to Sunday, 21 July 2554 23:34:33.709 (GMT)
        let max_millis = u64::MAX / NANOS_PER_MILLI;

        let result = Time::from_millis_since_unix_epoch(max_millis + 1);
        assert_matches!(result, Err(TimeInstantiationError::Overflow(_)));

        let result = Time::from_millis_since_unix_epoch(max_millis);
        assert_matches!(result, Ok(time) if time == Time::from_nanos_since_unix_epoch(18_446_744_073_709_000_000));
    }
}

#[test]
fn should_convert_from_system_time_and_back() {
    let system_time = SystemTime::now();
    let time: Time = system_time.try_into().unwrap();

    let system_time_nanos = system_time
        .duration_since(SystemTime::UNIX_EPOCH)
        .unwrap()
        .as_nanos() as u64;
    let time_nanos = time.as_nanos_since_unix_epoch();
    assert_eq!(system_time_nanos, time_nanos);

    let back: SystemTime = time.into();
    assert_eq!(system_time, back);
}
