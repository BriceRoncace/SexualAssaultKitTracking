package gov.idaho.isp.saktrack.controller.advice;

import gov.idaho.isp.mail.MailMessage;
import gov.idaho.isp.mail.Mailer;
import gov.idaho.isp.saktrack.exception.ErrorAware;
import gov.idaho.isp.saktrack.util.beanvalidation.BeanValidationUtils;
import gov.idaho.isp.saktrack.util.web.WebExceptionUtils;
import java.util.Set;
import javax.servlet.http.HttpServletRequest;

public class EmailExceptionHandler {
  public static final String OPTIONS_ATTR_NAME = "exceptionHandlingOptions";

  private String errorView = "error";
  private String errorEmailFrom = null;
  private String errorEmailTo = "cjisdevelopment@isp.idaho.gov";
  private String errorEmailSubject = "Application Exception";
  private boolean sendEmail = true;

  private Mailer mailer;

  public static class Options {
    private String errorView;
    private boolean sendErrorEmail;

    public Options() {}

    public Options(String errorView, boolean sendErrorEmail) {
      this.errorView = errorView;
      this.sendErrorEmail = sendErrorEmail;
    }

    public String getErrorView() {
      return errorView;
    }

    public void setErrorView(String errorView) {
      this.errorView = errorView;
    }

    public boolean shouldSendErrorEmail() {
      return sendErrorEmail;
    }

    public void setSendErrorEmail(boolean sendErrorEmail) {
      this.sendErrorEmail = sendErrorEmail;
    }
  }

  public String handleException(HttpServletRequest req, RuntimeException ex) {
    req.setAttribute("url", req.getRequestURI());
    req.setAttribute("errors", getErrors(ex));
    req.setAttribute("exception", ex);

    Options options = getExceptionHandlingOptions(req);
    if (options.shouldSendErrorEmail()) {
      sendExceptionDetails(req, ex);
    }
    return options.getErrorView();
  }

  private Options getExceptionHandlingOptions(HttpServletRequest req) {
    if (req.getAttribute(OPTIONS_ATTR_NAME) != null) {
      return (Options) req.getAttribute(OPTIONS_ATTR_NAME);
    }
    return new Options(errorView, sendEmail);
  }

  private Set<String> getErrors(RuntimeException ex) {
    if (ex instanceof ErrorAware) {
      return ((ErrorAware)ex).getErrors();
    }
    return BeanValidationUtils.getConstraintViolationErrorMessages(ex);
  }

  private void sendExceptionDetails(HttpServletRequest request, RuntimeException ex) {
    MailMessage message = new MailMessage();
    message.setMimeType(MailMessage.MimeType.TEXT);
    message.setFrom(errorEmailFrom);
    message.setTo(errorEmailTo);
    message.setSubject(errorEmailSubject);
    message.setBody(WebExceptionUtils.getExceptionMessage(request, ex) + ex);
    try {
      mailer.send(message);
    }
    catch (Exception ignore) {
    }
  }

  public void setMailer(Mailer mailer) {
    this.mailer = mailer;
  }

  public void setErrorView(String errorView) {
    this.errorView = errorView;
  }

  public void setErrorEmailFrom(String errorEmailFrom) {
    this.errorEmailFrom = errorEmailFrom;
  }

  public void setErrorEmailTo(String errorEmailTo) {
    this.errorEmailTo = errorEmailTo;
  }

  public void setErrorEmailSubject(String errorEmailSubject) {
    this.errorEmailSubject = errorEmailSubject;
  }

  public void setSendEmail(boolean sendEmail) {
    this.sendEmail = sendEmail;
  }
}