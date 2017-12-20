package gov.idaho.isp.saktrack.controller.advice;

import gov.idaho.isp.saktrack.propertyeditor.LocalDatePropertyEditor;
import gov.idaho.isp.saktrack.propertyeditor.LocalDateTimePropertyEditor;
import java.time.LocalDate;
import java.time.LocalDateTime;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.WebDataBinder;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.InitBinder;

@ControllerAdvice
public class GlobalInitBinder {
  private LocalDatePropertyEditor localDatePropertyEditor;
  private LocalDateTimePropertyEditor localDateTimePropertyEditor;

  @InitBinder
  public void initDateBinders(WebDataBinder binder) {
    binder.registerCustomEditor(LocalDate.class, localDatePropertyEditor);
    binder.registerCustomEditor(LocalDateTime.class, localDateTimePropertyEditor);
  }

  @Autowired
  public void setLocalDatePropertyEditor(LocalDatePropertyEditor localDatePropertyEditor) {
    this.localDatePropertyEditor = localDatePropertyEditor;
  }

  @Autowired
  public void setLocalDateTimePropertyEditor(LocalDateTimePropertyEditor localDateTimePropertyEditor) {
    this.localDateTimePropertyEditor = localDateTimePropertyEditor;
  }
}