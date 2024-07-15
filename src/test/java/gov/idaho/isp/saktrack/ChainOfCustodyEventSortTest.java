package gov.idaho.isp.saktrack;

import gov.idaho.isp.saktrack.domain.ChainOfCustodyEvent;
import org.junit.Assert;
import org.junit.Test;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.Month;
import java.util.ArrayList;
import java.util.List;

public class ChainOfCustodyEventSortTest {
  @Test
  public void chainOfCustodyEventsSortedByArrayIndexAndExplicitlyManagableByAdmin() {
    List<ChainOfCustodyEvent> list = new ArrayList<>();

    list.add(buildEvent(1L, LocalDate.of(2016, Month.SEPTEMBER, 20)));
    list.add(buildEvent(2L, LocalDate.of(2016, Month.SEPTEMBER, 20)));
    list.add(buildEvent(3L, LocalDate.of(2016, Month.SEPTEMBER, 20)));
    list.add(buildEvent(4L, LocalDate.of(2016, Month.SEPTEMBER, 20)));
    list.add(buildEvent(5L, LocalDate.of(2016, Month.SEPTEMBER, 20)));
    list.add(buildEvent(6L, LocalDate.of(2016, Month.SEPTEMBER, 20)));
    list.add(buildEvent(7L, LocalDate.of(2016, Month.SEPTEMBER, 20)));

    Assert.assertEquals(Long.valueOf(1), list.get(0).getId());
    Assert.assertEquals(Long.valueOf(7), list.get(6).getId());
  }

  private ChainOfCustodyEvent buildEvent(Long id, LocalDate eventDate) {
    ChainOfCustodyEvent e = new ChainOfCustodyEvent();
    e.setId(id);
    e.setEventDate(eventDate);
    e.setDigitalTimestamp(LocalDateTime.now());
    try {
      Thread.sleep(100L);
    }
    catch (Exception ignore) {
    }
    return e;
  }
}
