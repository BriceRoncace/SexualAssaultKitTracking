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

package gov.idaho.isp.saktrack.util.collection;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class SetUtils {

  public static <T> Set<T> of(T... items) {
    return Stream.of(items).collect(Collectors.toSet());
  }

  public static Set<Integer> of(int[] intArray) {
    return Arrays.stream(intArray).boxed().collect(Collectors.toSet());
  }

  /**
  * The returned set contains all elements that are contained by {@code set1} and not contained by {@code set2}.
  * {@code set2} may also contain elements not present in {@code set1}; these are simply ignored.
  */
  public static <T> Set<T> difference(final Set<? extends T> set1, final Set<? extends T> set2) {
    Set<T> diff = new HashSet<>(set1);
    diff.removeAll(set2);
    return diff;
  }

  /**
   * The returned set contains all of the unique elements of {@code set1} plus {@code set2}.
   */
  public static <T> Set<T> union(final Set<? extends T> set1, final Set<? extends T> set2) {
    Set<T> union = new HashSet<>(set1);
    union.addAll(set2);
    return union;
  }

  /**
   * The returned set contains all of the elements in {@code set1} that are present in {@code set2}.
   */
  public static <T> Set<T> intersection(final Set<? extends T> set1, final Set<? extends T> set2) {
    Set<T> intersection = new HashSet<>(set1);
    intersection.retainAll(set2);
    return intersection;
  }

  /**
   * The returned set contains all elements that are contained in either
   * {@code set1} or {@code set2} but not in both.
   */
  public static <T> Set<T> symmetricDifference(final Set<? extends T> set1, final Set<? extends T> set2) {
    return difference(union(set1, set2), intersection(set1, set2));
  }
}
