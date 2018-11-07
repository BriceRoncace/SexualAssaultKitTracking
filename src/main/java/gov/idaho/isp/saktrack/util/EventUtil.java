package gov.idaho.isp.saktrack.util;

import gov.idaho.isp.saktrack.domain.ChainOfCustodyEvent;
import gov.idaho.isp.saktrack.domain.ChainOfCustodyEvent.EventType;
import gov.idaho.isp.saktrack.domain.SexualAssaultKit;
import gov.idaho.isp.saktrack.domain.dto.EventDetails;
import gov.idaho.isp.saktrack.domain.organization.Organization;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

public class EventUtil {

  public static boolean hasMissingSendEvents(List<ChainOfCustodyEvent> events) {
    return !getMissingSendEvents(events).isEmpty();
  }

  public static List<MissingSendEvent> getMissingSendEvents(List<ChainOfCustodyEvent> events) {
    List<MissingSendEvent> missing = new ArrayList<>();
    List<ChainOfCustodyEvent> sent = getEventsByEventType(events, EventType.SEND);
    List<ChainOfCustodyEvent> received = getEventsByEventType(events, EventType.RECEIVE);

    received.stream().forEach((r) -> {
      boolean hasPairedEvent = hasPairedEventInList(r, sent);
      if (!hasPairedEvent) {
        missing.add(new MissingSendEvent(r.getFrom(), r.getTo()));
      }
    });

    return missing;
  }

  public static boolean validSendDate(SexualAssaultKit kit, Organization sender, Organization sendTo, EventDetails sendEventDetails) {
    if (isEventBeforeCreation(kit, sendEventDetails)) {
      return false;
    }

    ChainOfCustodyEvent constructedSendEventToValidate = buildEvent(sender, sendTo, sendEventDetails);
    List<ChainOfCustodyEvent> allReceiveEventsOnKit = getEventsByEventType(kit.getChainOfCustody(), EventType.RECEIVE);
    List<ChainOfCustodyEvent> allSendEventsOnKit = getEventsByEventType(kit.getChainOfCustody(), EventType.SEND);

    if (!allReceiveEventsOnKit.isEmpty()) {
      List<ChainOfCustodyEvent> senderReceiveEvents = allReceiveEventsOnKit.stream().filter(c -> c.getActorOrganization().equals(sender)).collect(Collectors.toList());
      if (!senderReceiveEvents.isEmpty()) {
        ChainOfCustodyEvent receipt = getEventByMinEventDate(senderReceiveEvents);
        if (receipt != null) {
          return isNewSendDateOnOrAfterEarliestReceiveDate(sendEventDetails, receipt);
        }
      }
    }

    if (hasAnUnpairedReceiveEventWithoutAnyCorrespondingSendEvents(constructedSendEventToValidate, allReceiveEventsOnKit, allSendEventsOnKit)) {
      ChainOfCustodyEvent unpairedReceiveEvent = getUnpairedReceiveEvent(constructedSendEventToValidate, allReceiveEventsOnKit);
      return isNewSendDateOnOrBeforeUnpairedReceiveDate(sendEventDetails, unpairedReceiveEvent);
    }
    else if (hasPairedReceivedAndSendEvents(constructedSendEventToValidate, allReceiveEventsOnKit, allSendEventsOnKit)) {
      if (everyReceiveHasAPairedSend(constructedSendEventToValidate, allReceiveEventsOnKit, allSendEventsOnKit)) {
        return true;
      }
      else {
        return isNewSendDateOnOrBeforeUnpairedReceiveDate(sendEventDetails, getUnpairedReceiveEvent(constructedSendEventToValidate, allReceiveEventsOnKit));
      }
    }

    return true;
  }

  public static boolean validReceiveDate(SexualAssaultKit kit, Organization receiver, Organization receivedFrom, EventDetails receiveEventDetails) {
    if (isEventBeforeCreation(kit, receiveEventDetails)) {
      return false;
    }

    ChainOfCustodyEvent constructedReceiveEvent = buildEvent(receivedFrom, receiver, receiveEventDetails);
    List<ChainOfCustodyEvent> allSendEventsOnKit = getEventsByEventType(kit.getChainOfCustody(), EventType.SEND);

    if (hasPairedEventInList(constructedReceiveEvent, allSendEventsOnKit)) {
      ChainOfCustodyEvent sendEvent = getPairedSendEvent(constructedReceiveEvent, allSendEventsOnKit);
      return isNewReceiveDateOnOrAfterPairedSendDate(receiveEventDetails, sendEvent);
    }

    return true;
  }

  private static boolean isEventBeforeCreation(SexualAssaultKit kit, EventDetails eventDetails) {
    List<ChainOfCustodyEvent> events = getEventsByEventType(kit.getChainOfCustody(), EventType.CREATE);
    if (events.isEmpty()) {
      return true;
    }

    ChainOfCustodyEvent createEvent = events.get(0);
    return eventDetails.getEventDate().isBefore(createEvent.getEventDate());
  }

  private static ChainOfCustodyEvent buildEvent(Organization from, Organization to, EventDetails eventDetails) {
    ChainOfCustodyEvent event = new ChainOfCustodyEvent();
    event.setFrom(from);
    event.setTo(to);
    event.setEventDate(eventDetails.getEventDate());
    return event;
  }

  private static List<ChainOfCustodyEvent> getEventsByEventType(List<ChainOfCustodyEvent> events, EventType eventType) {
    return events.stream().filter(e -> eventType == e.getEventType()).collect(Collectors.toList());
  }

  private static boolean hasPairedEventInList(ChainOfCustodyEvent event, List<ChainOfCustodyEvent> events) {
    return !getPairedEventsInList(event, events).isEmpty();
  }

  private static List<ChainOfCustodyEvent> getPairedEventsInList(ChainOfCustodyEvent event, List<ChainOfCustodyEvent> events) {
    return events.stream().filter(e -> Objects.equals(e.getFrom(), event.getFrom()) && Objects.equals(e.getTo(), event.getTo())).collect(Collectors.toList());
  }

  private static boolean isNewSendDateOnOrBeforeUnpairedReceiveDate(EventDetails sendEventDetails, ChainOfCustodyEvent receiveEvent) {
    return sendEventDetails.getEventDate().isEqual(receiveEvent.getEventDate()) || sendEventDetails.getEventDate().isBefore(receiveEvent.getEventDate());
  }

  private static boolean isNewSendDateOnOrAfterEarliestReceiveDate(EventDetails sendEventDetails, ChainOfCustodyEvent receiveEvent) {
    return isEventDetailsDateOnOrAfterChainOfCustodyEventDate(sendEventDetails, receiveEvent);
  }

  private static boolean isNewReceiveDateOnOrAfterPairedSendDate(EventDetails receiveEventDetails, ChainOfCustodyEvent sendEvent) {
    return isEventDetailsDateOnOrAfterChainOfCustodyEventDate(receiveEventDetails, sendEvent);
  }

  private static boolean isEventDetailsDateOnOrAfterChainOfCustodyEventDate(EventDetails eventDetails, ChainOfCustodyEvent event) {
  return eventDetails.getEventDate().isEqual(event.getEventDate()) || eventDetails.getEventDate().isAfter(event.getEventDate());
  }

  private static boolean hasPairedReceivedAndSendEvents(ChainOfCustodyEvent constructedEvent, List<ChainOfCustodyEvent> allReceiveEventsOnKit, List<ChainOfCustodyEvent> allSendEventsOnKit) {
    return hasPairedEventInList(constructedEvent, allReceiveEventsOnKit) && hasPairedEventInList(constructedEvent, allSendEventsOnKit);
  }

  private static boolean everyReceiveHasAPairedSend(ChainOfCustodyEvent constructedEvent, List<ChainOfCustodyEvent> allReceiveEventsOnKit, List<ChainOfCustodyEvent> allSendEventsOnKit){
    return getPairedEventsInList(constructedEvent, allReceiveEventsOnKit).size() == getPairedEventsInList(constructedEvent, allSendEventsOnKit).size();
  }

  private static ChainOfCustodyEvent getUnpairedReceiveEvent(ChainOfCustodyEvent constructedEvent, List<ChainOfCustodyEvent> allReceiveEventsOnKit) {
    return getEventByMaxEventDate(getPairedEventsInList(constructedEvent, allReceiveEventsOnKit));
  }

  private static ChainOfCustodyEvent getPairedSendEvent(ChainOfCustodyEvent constructedEvent, List<ChainOfCustodyEvent> allSendEventsOnKit) {
    return getEventByMinEventDate(getPairedEventsInList(constructedEvent, allSendEventsOnKit));
  }

  private static boolean hasAnUnpairedReceiveEventWithoutAnyCorrespondingSendEvents(ChainOfCustodyEvent constructedEvent, List<ChainOfCustodyEvent> received, List<ChainOfCustodyEvent> allSendEventsOnKit) {
   return hasPairedEventInList(constructedEvent, received) && !hasPairedEventInList(constructedEvent, allSendEventsOnKit);
  }

  private static ChainOfCustodyEvent getEventByMaxEventDate(List<ChainOfCustodyEvent> events) {
    if (events.size() > 1) {
      return Collections.max(events, Comparator.comparing(e -> e.getEventDate()));
    }
    else {
      return events.get(0);
    }
  }

  private static ChainOfCustodyEvent getEventByMinEventDate(List<ChainOfCustodyEvent> events) {
    if (events.size() > 1) {
      return Collections.min(events, Comparator.comparing(e -> e.getEventDate()));
    }
    else {
      return events.get(0);
    }
  }


}
