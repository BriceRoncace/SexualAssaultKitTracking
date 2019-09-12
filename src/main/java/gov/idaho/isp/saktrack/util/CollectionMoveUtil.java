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

import java.util.List;

public class CollectionMoveUtil {
  public enum MoveDirection {
    UP, DOWN
  };

  public static <E> void move(List<E> list, E item, MoveDirection direction) {
    if (MoveDirection.UP.equals(direction)) {
      moveUp(list, item);
    }
    if (MoveDirection.DOWN.equals(direction)) {
      moveDown(list, item);
    }
  }

  //rearrange elements within a list
  private static <E> void moveUp(List<E> list, E item) {
    int index = list.indexOf(item);
    if (index != 0) {
      int beforeIndex = index - 1;
      swap(list, index, beforeIndex);
    }
  }

  private static <E> void moveDown(List<E> list, E item) {
    int index = list.indexOf(item);
    int size = list.size() - 1;
    if (index != size) {
      int afterIndex = index + 1;
      swap(list, index, afterIndex);
    }
  }

  private static <E> void swap(List<E> list, int index1, int index2) {
    E tmp = list.get(index1);
    list.set(index1, list.get(index2));
    list.set(index2, tmp);
  }
}
