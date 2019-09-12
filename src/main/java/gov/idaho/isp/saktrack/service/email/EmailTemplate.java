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

package gov.idaho.isp.saktrack.service.email;

import gov.idaho.isp.saktrack.service.TemplateEvaluator.Template;

public enum EmailTemplate implements Template {
  PASSWORD_RESET("Sexual Assault Kit Tracking password reset request", "/email/password-reset.html"),
  SUBMISSION_DUE_WARNING("Sexual Assault Kit Submission Deadline Approaching—Action Needed", "/email/le-kit-notification.html"),
  SUBMISSION_OVERDUE("PAST DUE—Sexual Assault Kit Submission Deadline has Passed—Your Immediate Action Requested", "/email/le-kit-notification.html"),
  DESTRUCTION_DATE_NEEDED("Sexual Assault Kits missing destruction date", "/email/destruction-date-needed.html"),
  DESTRUCTION_REMINDER("Upcoming Destruction of Sexual Assault Kit #%s", "/email/destruction-reminder.html"),
  NEW_USER_REGISTRATION("New User Registration", "/email/new-user-registration.html"),
  ATTORNEY_NOTIFICATION("Sexual Assault Kit from %1s (case #%2s) needs review", "/email/attorney-review-needed.html"),
  ATTORNEY_REVIEW_COMPLETE("Sexual Assault Kit %1s (case #%2s) has been reviewed", "/email/attorney-review-complete.html"),
  LAW_ENFORCEMENT_INCOMING_KIT("Sexual Assault Kit has been marked as sent", "/email/incoming-kit-notification.html");

  private final String subject;
  private final String classpathResource;

  private EmailTemplate(String subject, String classpathResource) {
    this.subject = subject;
    this.classpathResource = classpathResource;
  }

  public String getSubject() {
    return subject;
  }

  @Override
  public String getClasspathResourceName() {
    return classpathResource;
  }
}