package gov.idaho.isp.saktrack.util;

import java.util.List;
import java.util.function.Function;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;

public class PagingUtils {

  public static <T> Page<T> getFirstPageIfRequestedIsBlank(Function<Pageable, Page<T>> getPageFunction, Pageable pageable) {
    Page<T> page = getPageFunction.apply(pageable);
    int requestedPage = pageable.getPageNumber();
    if (requestedPage != 0 && pageNumberDoesNotExist(requestedPage, page)) {
      return getPageFunction.apply(PageRequest.of(0, pageable.getPageSize(), pageable.getSort()));
    }
    return page;
  }

  private static boolean pageNumberDoesNotExist(int pageNumber, Page page) {
    return pageNumber + 1 > page.getTotalPages();
  }

  public static <T, R> Page<R> toNewPage(Page<T> page, List<R> newPageContent) {
    return new PageImpl<>(newPageContent, PageRequest.of(page.getNumber(), page.getSize(), page.getSort()), page.getTotalElements());
  }

  public static String getSingleOrderBy(SortWrapper sortWrapper) {
    if (sortWrapper == null) {
      return "";
    }

    Sort sort = sortWrapper.unwrap();
    if (sort != null && sort.iterator() != null) {
      Sort.Order order = sort.iterator().next();
      return order.getProperty() + "," + order.getDirection();
    }

    return "";
  }
}
