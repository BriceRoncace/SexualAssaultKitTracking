package gov.idaho.isp.saktrack.controller.advice;

import gov.idaho.isp.saktrack.controller.advice.LoadEventDetailsAdvice.LoadEventDetails;
import gov.idaho.isp.saktrack.dto.CreateKitEventDetails;
import gov.idaho.isp.saktrack.dto.EventDetails;
import gov.idaho.isp.saktrack.service.RangeParser;
import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ModelAttribute;

@ControllerAdvice(annotations = LoadEventDetails.class)
public class LoadEventDetailsAdvice {

  @Target(ElementType.TYPE)
  @Retention(RetentionPolicy.RUNTIME)
  @Documented
  public @interface LoadEventDetails {}

  private final RangeParser rangeParser;

  public LoadEventDetailsAdvice(RangeParser rangeParser) {
    this.rangeParser = rangeParser;
  }

  @ModelAttribute
  public CreateKitEventDetails prepareCreateKitEventDetails(CreateKitEventDetails eventDetails, BindingResult ignore) {
    return parseSerialNubers(eventDetails);
  }

  @ModelAttribute
  public EventDetails prepareEventDetails(EventDetails eventDetails, BindingResult ignore) {
    return parseSerialNubers(eventDetails);
  }

  private <T extends EventDetails> T parseSerialNubers(T eventDetails) {
    try {
      eventDetails.setSerialNumberList(rangeParser.parse(eventDetails.getSerialNumbers()));
    }
    catch (Exception ignore) {}
    return eventDetails;
  }
}