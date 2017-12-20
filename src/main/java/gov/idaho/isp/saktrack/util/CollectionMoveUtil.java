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
