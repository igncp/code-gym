use regex::Regex;
use std::cmp::Ordering;
use std::collections::HashMap;

type Minute = usize;
type Minutes = usize;
type MinuteToSleptMinutes = HashMap<Minute, Minutes>;
type GuardID = usize;
type GuardIDToSleptMinutes = HashMap<GuardID, MinuteToSleptMinutes>;

#[derive(Debug, PartialEq, Eq)]
pub enum GuardAction {
  WakeUp,
  FallAsleep,
  BeginShift(GuardID),
}

#[derive(Debug, PartialEq, Eq)]
pub struct LogEntry {
  pub month: usize,
  pub day: usize,
  pub hour: usize,
  pub minute: usize,
  pub action: GuardAction,
}

impl LogEntry {
  pub fn parse_string(full_str: &String) -> LogEntry {
    let main_reg = Regex::new(r"^\[1518-(.+?)-(.+?) ([^ ]+?):(.+?)\] (.*?)$").unwrap();
    let caps = main_reg.captures(full_str).unwrap();

    fn get_action(action_str: String) -> GuardAction {
      let wake_up_reg = Regex::new(r"^wakes up$").unwrap();
      let falls_asleep_reg = Regex::new(r"^falls asleep$").unwrap();
      let begins_shift_reg = Regex::new(r"^Guard #(.*?) begins shift$").unwrap();

      if wake_up_reg.is_match(action_str.as_str()) {
        return GuardAction::WakeUp;
      }

      if falls_asleep_reg.is_match(action_str.as_str()) {
        return GuardAction::FallAsleep;
      }

      let begins_caps = begins_shift_reg.captures(&action_str).unwrap();
      let guard_id = begins_caps
        .get(1)
        .unwrap()
        .as_str()
        .parse::<usize>()
        .unwrap();

      GuardAction::BeginShift(guard_id)
    }

    let action = caps.get(5).unwrap().as_str().parse::<String>().unwrap();

    LogEntry {
      month: caps.get(1).unwrap().as_str().parse::<usize>().unwrap(),
      day: caps.get(2).unwrap().as_str().parse::<usize>().unwrap(),
      minute: caps.get(4).unwrap().as_str().parse::<usize>().unwrap(),
      hour: caps.get(3).unwrap().as_str().parse::<usize>().unwrap(),
      action: get_action(action),
    }
  }
}

impl Ord for LogEntry {
  fn cmp(&self, other: &LogEntry) -> Ordering {
    match self.month.cmp(&other.month) {
      Ordering::Equal => match self.day.cmp(&other.day) {
        Ordering::Equal => match self.hour.cmp(&other.hour) {
          Ordering::Equal => self.minute.cmp(&other.minute),
          v => v,
        },
        v => v,
      },
      v => v,
    }
  }
}

impl PartialOrd for LogEntry {
  fn partial_cmp(&self, other: &LogEntry) -> Option<Ordering> {
    Some(self.cmp(other))
  }
}

pub fn build_guard_id_to_slept_minutes_map(entries: &Vec<LogEntry>) -> GuardIDToSleptMinutes {
  let mut result: GuardIDToSleptMinutes = HashMap::new();

  let mut current_guard_id: Option<usize> = None;
  let mut current_time: Option<(usize, usize)> = None;
  let mut current_day: Option<(usize, usize)> = None;

  for entry in entries {
    if current_day != Some((entry.month, entry.day)) {
      current_day = None;
      current_time = None;
    }

    match entry.action {
      GuardAction::BeginShift(guard_id) => {
        match result.get(&guard_id) {
          None => {
            let mut empty = HashMap::new() as MinuteToSleptMinutes;

            result.insert(guard_id, empty);
          }
          _ => {}
        };

        current_guard_id = Some(guard_id);
      }
      GuardAction::FallAsleep => {
        current_time = match entry.hour {
          0 => Some((entry.hour, entry.minute)),
          _ => Some((0, 0)),
        };
        current_day = Some((entry.month, entry.day));
      }
      GuardAction::WakeUp => {
        if current_day.is_none() || current_time.is_none() || current_guard_id.is_none() {
          panic!(
            "Unexpected state: {}, {}, {}",
            current_day.is_none(),
            current_time.is_none(),
            current_guard_id.is_none()
          );
        }

        match entry.hour {
          0 => {
            let current_minute = current_time.unwrap().1;
            let guard_id_val = current_guard_id.unwrap();
            let mut guard_map = result.get_mut(&guard_id_val).unwrap();

            for minute in current_minute..entry.minute {
              let existing_minute = match guard_map.get(&minute) {
                Some(val) => val + 1,
                None => 1,
              };

              guard_map.insert(minute, existing_minute);
            }
          }
          _ => {}
        }
      }
    }
  }

  result
}

pub fn get_guard_id_with_most_sleeping_minutes(map: &GuardIDToSleptMinutes) -> usize {
  let mut current_match = (0, 0);

  for (guard_id, minutes_map) in map.iter() {
    let mut result_for_guard = 0;

    for (_, count) in minutes_map.iter() {
      result_for_guard += *count;
    }

    if current_match.1 < result_for_guard {
      current_match = (*guard_id, result_for_guard);
    }
  }

  return current_match.0;
}

pub fn get_most_slept_minute_for_guard(map: &GuardIDToSleptMinutes, guard_id: usize) -> usize {
  let mut current_match = (0, 0);

  for (minute, count) in map.get(&guard_id).unwrap().iter() {
    if *count > current_match.1 {
      current_match = (*minute, *count);
    }
  }

  return current_match.0;
}

pub fn get_guard_with_most_sleep_on_same_minute(
  map: &GuardIDToSleptMinutes,
) -> (GuardID, Minute, Minutes) {
  let mut current_match = (0, 0, 0);

  for (guard_id, minutes_map) in map.iter() {
    for (minute, minutes_count) in minutes_map.iter() {
      if current_match.2 < *minutes_count {
        current_match = (*guard_id, *minute, *minutes_count);
      }
    }
  }

  return current_match;
}

#[cfg(test)]
mod tests {
  use super::*;

  fn get_example_data() -> Vec<LogEntry> {
    vec![
      LogEntry::parse_string(&"[1518-11-01 00:00] Guard #10 begins shift".to_string()),
      LogEntry::parse_string(&"[1518-11-01 00:05] falls asleep".to_string()),
      LogEntry::parse_string(&"[1518-11-01 00:25] wakes up".to_string()),
      LogEntry::parse_string(&"[1518-11-01 00:30] falls asleep".to_string()),
      LogEntry::parse_string(&"[1518-11-01 00:55] wakes up".to_string()),
      LogEntry::parse_string(&"[1518-11-01 23:08] Guard #99 begins shift".to_string()),
      LogEntry::parse_string(&"[1518-11-02 00:40] falls asleep".to_string()),
      LogEntry::parse_string(&"[1518-11-02 00:50] wakes up".to_string()),
      LogEntry::parse_string(&"[1518-11-03 00:05] Guard #10 begins shift".to_string()),
      LogEntry::parse_string(&"[1518-11-03 00:24] falls asleep".to_string()),
      LogEntry::parse_string(&"[1518-11-03 00:29] wakes up".to_string()),
      LogEntry::parse_string(&"[1518-11-04 00:02] Guard #99 begins shift".to_string()),
      LogEntry::parse_string(&"[1518-11-04 00:36] falls asleep".to_string()),
      LogEntry::parse_string(&"[1518-11-04 00:46] wakes up".to_string()),
      LogEntry::parse_string(&"[1518-11-05 00:03] Guard #99 begins shift".to_string()),
      LogEntry::parse_string(&"[1518-11-05 00:45] falls asleep".to_string()),
      LogEntry::parse_string(&"[1518-11-05 00:55] wakes up".to_string()),
    ]
  }

  #[test]
  fn test_log_entry_parse_string() {
    assert_eq!(
      LogEntry::parse_string(&"[1518-04-22 00:52] wakes up".to_string()),
      LogEntry {
        month: 4,
        day: 22,
        hour: 0,
        minute: 52,
        action: GuardAction::WakeUp
      }
    );
    assert_eq!(
      LogEntry::parse_string(&"[1518-03-11 23:56] Guard #547 begins shift".to_string()),
      LogEntry {
        month: 3,
        day: 11,
        hour: 23,
        minute: 56,
        action: GuardAction::BeginShift(547)
      }
    );
    assert_eq!(
      LogEntry::parse_string(&"[1518-06-08 00:39] falls asleep".to_string()),
      LogEntry {
        month: 6,
        day: 8,
        hour: 0,
        minute: 39,
        action: GuardAction::FallAsleep
      }
    );
  }

  #[test]
  fn test_log_entry_sort() {
    let mut res = vec![
      LogEntry::parse_string(&"[1518-01-15 23:02] Guard #547 begins shift".to_string()),
      LogEntry::parse_string(&"[1518-01-10 23:01] Guard #547 begins shift".to_string()),
      LogEntry::parse_string(&"[1518-04-30 23:04] Guard #547 begins shift".to_string()),
      LogEntry::parse_string(&"[1518-02-01 23:03] Guard #547 begins shift".to_string()),
      LogEntry::parse_string(&"[1518-04-30 00:05] Guard #547 begins shift".to_string()),
      LogEntry::parse_string(&"[1518-04-30 23:06] Guard #547 begins shift".to_string()),
    ];

    res.sort();

    let minutes: Vec<usize> = res.iter().map(|x| x.minute).collect();

    assert_eq!(minutes, vec![1, 2, 3, 5, 4, 6]);
  }

  #[test]
  fn test_flow_1() {
    let res = get_example_data();
    let guard_id_to_slept_minutes_map = build_guard_id_to_slept_minutes_map(&res);
    let guard_id_with_most_sleeping_minutes =
      get_guard_id_with_most_sleeping_minutes(&guard_id_to_slept_minutes_map);

    let most_slept_minute_for_guard = get_most_slept_minute_for_guard(
      &guard_id_to_slept_minutes_map,
      guard_id_with_most_sleeping_minutes,
    );
    let result = most_slept_minute_for_guard * guard_id_with_most_sleeping_minutes;

    assert_eq!(result, 240);
  }

  #[test]
  fn test_get_guard_with_most_sleep_on_same_minute_example_2() {
    let res = get_example_data();
    let guard_id_to_slept_minutes_map = build_guard_id_to_slept_minutes_map(&res);
    let result = get_guard_with_most_sleep_on_same_minute(&guard_id_to_slept_minutes_map);

    assert_eq!(result, (99, 45, 3));
    assert_eq!(result.0 * result.1, 4455);
  }
}
