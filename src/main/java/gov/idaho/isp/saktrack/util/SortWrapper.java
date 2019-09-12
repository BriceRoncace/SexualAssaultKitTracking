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

package gov.idaho.isp.saktrack.util;

import org.apache.commons.lang3.StringUtils;
import org.springframework.data.domain.Sort;

public class SortWrapper {
  private final Sort sort;

  private SortWrapper(Sort sort) {
    this.sort = sort;
  }

  public static SortWrapper valueOf(String s) {
    if (StringUtils.isBlank(s)) {
      return null;
    }
    return new SortWrapper(parseAsSort(s));
  }

  private static Sort parseAsSort(String s) {
    String[] propAndDir = s.split(",");

    String prop = propAndDir[0];
    String dir = (propAndDir.length > 1) ? propAndDir[1] : "ASC";

    return Sort.by(Sort.Direction.valueOf(dir), prop);
  }

  public Sort unwrap() {
    return sort;
  }

  @Override
  public String toString() {
    return "SortWrapper{" + "sort=" + sort + '}';
  }
}
