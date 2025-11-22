/*
 * Copyright 2017 Idaho State Police.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package gov.idaho.isp.saktrack.service;

import gov.idaho.isp.saktrack.exception.SexualAssaultKitTrackingException;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

@Service
public class SerialNumberFormatterImpl implements SerialNumberFormatter {
  private final String prefix;
  private final int length;
  private final Pattern fullFormat;
  private final Pattern prefixFormat;
  private final Pattern digitsFormat;

  public SerialNumberFormatterImpl(@Value("${serial.number.prefix}") String prefix, @Value("${serial.number.length}") int length) {
    this.prefix = prefix != null ? prefix : "";
    this.length = length;

    String escapedPrefix = Pattern.quote(this.prefix);
    this.fullFormat = Pattern.compile("^" + escapedPrefix + "\\d{" + length + "}$");
    this.prefixFormat = Pattern.compile("^" + escapedPrefix + "(\\d{1," + length + "})$");
    this.digitsFormat = Pattern.compile("^\\d{1," + length + "}$");
  }

  @Override
  public String format(int serialNumber) {
    return format(String.valueOf(serialNumber));
  }

  @Override
  public String format(String input) {
    if (input == null) {
      throw new SexualAssaultKitTrackingException("Serial number cannot be null");
    }

    input = input.trim();

    // Case 1: Already correct
    if (fullFormat.matcher(input).matches()) {
      return input;
    }

    // Case 2: Prefix + 1..length digits
    Matcher prefixed = prefixFormat.matcher(input);
    if (prefixed.matches()) {
      int number = Integer.parseInt(prefixed.group(1));
      ensureRange(number);
      return prefix + String.format("%0" + length + "d", number);
    }

    // Case 3: Digits only
    if (digitsFormat.matcher(input).matches()) {
      int number = Integer.parseInt(input);
      ensureRange(number);
      return prefix + String.format("%0" + length + "d", number);
    }

    throw new SexualAssaultKitTrackingException("Could not format serial number [" + input + "]");
  }

  private void ensureRange(int number) {
    if (number < 0 || number > maxValue()) {
      throw new IllegalArgumentException("Serial number must be between 0 and " + maxValue());
    }
  }

  private int maxValue() {
    return (int) Math.pow(10, length) - 1;
  }

  @Override
  public boolean isValid(String input) {
    try {
      format(input);
      return true;
    }
    catch (Exception ex) {
      return false;
    }
  }
}
