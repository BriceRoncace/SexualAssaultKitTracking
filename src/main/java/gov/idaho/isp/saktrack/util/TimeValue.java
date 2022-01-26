package gov.idaho.isp.saktrack.util;

import java.util.concurrent.TimeUnit;

public class TimeValue {
  private final long value;
  private final TimeUnit timeUnit;

  private TimeValue(long value, TimeUnit unit) {
    this.value = value;
    this.timeUnit = unit;
  }

  public static TimeValue of(long value, TimeUnit unit) {
    return new TimeValue(value, unit);
  }

  public static TimeValue days(long days) {
    return TimeValue.of(days, TimeUnit.DAYS);
  }

  public static TimeValue hours(long hours) {
    return TimeValue.of(hours, TimeUnit.HOURS);
  }

  public static TimeValue minutes(long minutes) {
    return TimeValue.of(minutes, TimeUnit.MINUTES);
  }

  public static TimeValue seconds(long seconds) {
    return TimeValue.of(seconds, TimeUnit.SECONDS);
  }

  public static TimeValue milliseconds(long milliseconds) {
    return TimeValue.of(milliseconds, TimeUnit.MILLISECONDS);
  }

  public static TimeValue microseconds(long microseconds) {
    return TimeValue.of(microseconds, TimeUnit.MICROSECONDS);
  }

  public static TimeValue nanoseconds(long nanoseconds) {
    return TimeValue.of(nanoseconds, TimeUnit.NANOSECONDS);
  }

  public long getValue() {
    return value;
  }

  public TimeUnit getTimeUnit() {
    return timeUnit;
  }

  public TimeValue convertTo(TimeUnit unit) {
    return TimeValue.of(unit.convert(value, timeUnit), unit);
  }

  public TimeValue toDays() {
    return convertTo(TimeUnit.DAYS);
  }

  public TimeValue toHours() {
    return convertTo(TimeUnit.HOURS);
  }

  public TimeValue toMinutes() {
    return convertTo(TimeUnit.MINUTES);
  }

  public TimeValue toSeconds() {
    return convertTo(TimeUnit.SECONDS);
  }

  public TimeValue toMillis() {
    return convertTo(TimeUnit.MILLISECONDS);
  }

  public TimeValue toMicros() {
    return convertTo(TimeUnit.MICROSECONDS);
  }

  public TimeValue toNanos() {
    return convertTo(TimeUnit.NANOSECONDS);
  }

  @Override
  public String toString() {
    return "TimeValue{" + "value=" + value + ", timeUnit=" + timeUnit + '}';
  }

  public String prettyPrint() {
    String unit = timeUnit.name().toLowerCase();
    return value + " " + (value == 1L ? unit.substring(0, unit.length() - 1) : unit);
  }
}
