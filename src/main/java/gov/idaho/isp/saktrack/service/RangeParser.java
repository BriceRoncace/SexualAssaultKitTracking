package gov.idaho.isp.saktrack.service;

import java.util.List;

public interface RangeParser {
  /**
   * Parses String input into a list of numeric Strings.  The range String should
   * contain whitespace or comma separated numeric values optionally containing
   * ranged sequences denoted by the dash character.
   *
   * Example A: 100-160\n175 176 185-202
   * Example B: 100, 103, 105, 107-199
   * Example C: 100 103 105, 107-199
   */
  List<String> parse(String input);
}
