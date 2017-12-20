package gov.idaho.isp.saktrack.controller.advice;

import javax.servlet.http.HttpServletRequest;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;

@ControllerAdvice
public class GlobalExceptionHandler {
  private EmailExceptionHandler exceptionHandler;

  @ExceptionHandler(RuntimeException.class)
  public String handleException(HttpServletRequest request, RuntimeException ex) {
    return exceptionHandler.handleException(request, ex);
  }

  @Autowired
  public void setExceptionHandler(EmailExceptionHandler exceptionHandler) {
    this.exceptionHandler = exceptionHandler;
  }
}