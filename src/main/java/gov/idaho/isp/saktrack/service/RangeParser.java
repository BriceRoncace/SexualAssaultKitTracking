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
