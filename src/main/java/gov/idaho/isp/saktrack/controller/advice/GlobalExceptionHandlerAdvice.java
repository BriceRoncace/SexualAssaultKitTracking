package gov.idaho.isp.saktrack.controller.advice;

import gov.idaho.isp.saktrack.util.exception.EmailExceptionHandler;
import javax.servlet.http.HttpServletRequest;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;

@ControllerAdvice
public class GlobalExceptionHandlerAdvice {
  private final EmailExceptionHandler exceptionHandler;

  public GlobalExceptionHandlerAdvice(EmailExceptionHandler exceptionHandler) {
    this.exceptionHandler = exceptionHandler;
  }

  @ExceptionHandler(RuntimeException.class)
  public String handleException(HttpServletRequest request, RuntimeException ex) {
    return exceptionHandler.handleException(request, ex);
  }
}