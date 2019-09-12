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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;
import java.util.stream.Collectors;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;

@Service
public class RangeParserImpl implements RangeParser {
  private final SerialNumberFormatter serialNumberFormatter;

  public RangeParserImpl(SerialNumberFormatter serialNumberFormatter) {
    this.serialNumberFormatter = serialNumberFormatter;
  }

  @Override
  public List<String> parse(String input) {
    Set<String> result = new TreeSet<>();
    for (String token : splitByWhitespaceOrComma(input)) {
      if (isRange(token)) {
        result.addAll(asRange(token));
      }
      else if (isNumeric(token)) {
        result.add(serialNumberFormatter.format(token));
      }
      else {
        throw new IllegalArgumentException("Cannot parse range input String [" + input + "]. Input should be separated by whitespace or comma and contain only numeric values or a single dash designating an inclusive sequential range of numbers.");
      }
    }

    return result.stream().map(Object::toString).collect(Collectors.toList());
  }

  private List<String> splitByWhitespaceOrComma(String input) {
    if (StringUtils.isBlank(input)) {
      return Collections.emptyList();
    }
    String[] splitStrings = input.trim().split("[\\s,]+");
    return asList(splitStrings);
  }

  private boolean isRange(String token) {
    String[] rangePoints = token.trim().split("-");
    if (rangePoints.length == 2) {
      return isNumeric(rangePoints[0]) && isNumeric(rangePoints[1]);
    }
    return false;
  }

  private boolean isNumeric(String token) {
    return StringUtils.isNumeric(token);
  }

  private List<String> asRange(String token) {
    String[] rangePoints = token.trim().split("-");
    return buildRangeInclusive(rangePoints[0],  rangePoints[1]);
  }

  private List<String> asList(String[] array) {
    return Arrays.stream(array)
      .map(s -> s != null ? s.trim() : null)
      .collect(Collectors.toList());
  }

  private List<String> buildRangeInclusive(String start, String end) {
    if (StringUtils.isNumeric(start) && StringUtils.isNumeric(end)) {
      int startInt = Integer.valueOf(start);
      int endInt = Integer.valueOf(end);
      List<String> range = new ArrayList<>();
      for (int i = startInt; i <= endInt; i++) {
        range.add(serialNumberFormatter.format(i));
      }
      return range;
    }

    return Collections.emptyList();
  }
}
