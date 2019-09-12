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

package gov.idaho.isp.saktrack.util.exception;

import gov.idaho.isp.saktrack.exception.ErrorAware;
import gov.idaho.isp.saktrack.mailer.MailMessage;
import gov.idaho.isp.saktrack.mailer.Mailer;
import gov.idaho.isp.saktrack.mailer.MimeType;
import gov.idaho.isp.saktrack.util.beanvalidation.BeanValidationUtils;
import gov.idaho.isp.saktrack.util.web.WebExceptionUtils;
import java.util.Set;
import javax.servlet.http.HttpServletRequest;

public class EmailExceptionHandler {
  public static final String OPTIONS_ATTR_NAME = "exceptionHandlingOptions";

  private String errorView = "error";
  private String errorEmailFrom;
  private String errorEmailTo;
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
    message.setMimeType(MimeType.TEXT);
    message.setFrom(errorEmailFrom);
    message.setTo(errorEmailTo);
    message.setSubject(errorEmailSubject);
    message.setText(WebExceptionUtils.getExceptionMessage(request, ex) + ex);
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