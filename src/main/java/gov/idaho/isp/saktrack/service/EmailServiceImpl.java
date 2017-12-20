package gov.idaho.isp.saktrack.service;

import gov.idaho.isp.mail.MailMessage;
import gov.idaho.isp.mail.Mailer;
import gov.idaho.isp.saktrack.SexualAssaultKit;
import gov.idaho.isp.saktrack.dto.EventDetails;
import gov.idaho.isp.saktrack.organization.Organization;
import gov.idaho.isp.saktrack.organization.OrganizationRepository;
import gov.idaho.isp.saktrack.user.User;
import gov.idaho.isp.saktrack.user.organization.AbstractOrganizationUser;
import gov.idaho.isp.saktrack.user.organization.LawEnforcementUser;
import gov.idaho.isp.saktrack.user.organization.OrganizationUser;
import gov.idaho.isp.saktrack.user.password.PasswordToken;
import gov.idaho.isp.saktrack.user.persistence.OrganizationUserRepository;
import gov.idaho.isp.saktrack.util.MilestoneUtils;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

@Service
public class EmailServiceImpl implements EmailService {
  private static final Logger LOGGER = LoggerFactory.getLogger(EmailServiceImpl.class);
  private final String PASSWORD_RESET_SUBJECT = "Sexual Assault Kit Tracking password reset request";
  private final String SAK_SUBMISSION_WARNING_SUBJECT = "Sexual Assault Kit Submission Deadline Approaching—Your Action Needed";
  private final String SAK_SUBMISSION_VIOLATION_SUBJECT = "PAST DUE—Sexual Assault Kit Submission Deadline has Passed—Your Immediate Action Requested";
  private final String SAK_PLANNED_DESTRUCTION_DATE_REMINDER_SUBJECT = "Sexual Assault Kits missing destruction date";
  private final String PLANNED_DESTRUCTION_SUBJECT = "Upcoming Destruction of Sexual Assault Kit #%s";
  private final String NEW_USER_REGISTERED_SUBJECT = "New User Registration";
  private final String ATTORNEY_NOTIFICATION_SUBJECT = "Sexual Assault Kit from %1s (case #%2s) needs review.";
  private final String ATTORNEY_HAS_REVIEWED_SUBJECT = "Sexual Assault Kit %1s (case #%2s) has been reviewed.";
  private final String LAW_ENFORCEMENT_NOTIFICATION = "Sexual Assault Kit has been marked as sent.";

  @Value(value = "${application.uri}")
  private String uri;
  private Mailer mailer;

  private OrganizationRepository organizationRepository;
  private OrganizationUserRepository organizaitonUserRepository;

  @Override
  public boolean sendPasswordResetEmail(PasswordToken token, OrganizationUser user) {
    StringBuilder message = new StringBuilder();
    message.append("You are receiving this email because you requested a password reset. Please click on the link below to continue.<br/><br/>");
    message.append("<a href='").append(uri).append("/reset/password?request=").append(token.getToken()).append("'>");
    message.append("Reset Password for ").append(user.getUsername()).append("</a>");

    MailMessage msg = new MailMessage();
    msg.setTo(user.getEmail());
    msg.setSubject(PASSWORD_RESET_SUBJECT);
    msg.setBody(message.toString());
    return sendEmail(msg);
  }

  @Override
  public void sendKitsNeedLeActionReminderEmail(SexualAssaultKit kit) {
    List<AbstractOrganizationUser> adminUsers = getOrgUsersByIdAndFilter(kit.getMedicalDetails().getRequestingLeAgency().getId(), (u) -> isEligibleOrgAdmin(u));
    List<String> addresses = adminUsers.stream().map(User::getEmail).collect(Collectors.toList());

    MailMessage msg = new MailMessage();
    msg.setTo(addresses);
    msg.setSubject(SAK_SUBMISSION_WARNING_SUBJECT);
    msg.setBody(buildLeNotificationMessage(kit));
    sendEmail(msg);
  }

  @Override
  public void sendKitPastSubmitEmail(SexualAssaultKit kit) {
    List<AbstractOrganizationUser> adminUsers = getOrgUsersByIdAndFilter(kit.getMedicalDetails().getRequestingLeAgency().getId(), (u) -> isEligibleOrgAdmin(u));
    List<String> addresses = adminUsers.stream().map(User::getEmail).collect(Collectors.toList());

    MailMessage msg = new MailMessage();
    msg.setTo(addresses);
    msg.setSubject(SAK_SUBMISSION_VIOLATION_SUBJECT);
    msg.setBody(buildLeNotificationMessage(kit));
    sendEmail(msg);
  }

  @Override
  public void sendPlannedDestructionDateReminder(List<SexualAssaultKit> kits) {
    List<AbstractOrganizationUser> adminUsers = getOrgUsersByIdAndFilter(kits.get(0).getMedicalDetails().getRequestingLeAgency().getId(), (u) -> isEligibleOrgAdmin(u));
    List<String> addresses = adminUsers.stream().map(User::getEmail).collect(Collectors.toList());

    StringBuilder message = new StringBuilder();
    message
      .append("<style type=\"text/css\">th, td { border-bottom: 1px solid black; padding: 8px 16px; text-align: left; }</style>")
      .append("The following sexual assault kits are required to have a planned destruction date entered in IKTS by your agency. ")
      .append("Sexual assault evidence retention timeframes are mandated by Idaho statute 67-2919. ")
      .append("Please go to <a href=\"").append(uri).append("\">").append(uri).append("</a> in order to log in and complete this task. ")
      .append("Thank you for addressing this critical task and for your dedication to this important initiative.<br/><br/>")
      .append("<table><thead><tr><th>Kit Serial #</th><th>LE Case #</th></tr></thead><tbody>");

    kits.forEach(kit -> {
      message
        .append("<tr><td>")
        .append(kit.getSerialNumber())
        .append("</td><td>")
        .append(kit.getLeDetails().getCaseNumber())
        .append("</td></tr>");
    });

    message
      .append("</tbody></table>");

    MailMessage msg = new MailMessage();
    msg.setTo(addresses);
    msg.setSubject(SAK_PLANNED_DESTRUCTION_DATE_REMINDER_SUBJECT);
    msg.setBody(message.toString());
    sendEmail(msg);
  }

  @Override
  public void sendUpcomingKitDestructionEmail(SexualAssaultKit kit) {
    List<AbstractOrganizationUser> adminUsers = getOrgUsersByIdAndFilter(kit.getMedicalDetails().getRequestingLeAgency().getId(), (u) -> isEligibleOrgAdmin(u));
    List<String> addresses = adminUsers.stream().map(User::getEmail).collect(Collectors.toList());
    if (addresses == null || addresses.isEmpty()) {
      return;
    }

    StringBuilder message = new StringBuilder();
    message
      .append("You are receiving this email because sexual assault kit #").append(kit.getSerialNumber())
      .append("</a> has a planned destruction date of ")
      .append(kit.getLeDetails().getPlannedDestructionDate().format(DateTimeFormatter.ofPattern("MM/dd/yyyy"))).append(". ")
      .append("The victim requested to be notified in writing that the kit is set to be destroyed. Per Idaho Statute, upon written request, the victim shall be notified when there is any planned destruction of a sexual assault evidence kit.");

    MailMessage msg = new MailMessage();
    msg.setTo(addresses);
    msg.setSubject(String.format(PLANNED_DESTRUCTION_SUBJECT, kit.getSerialNumber()));
    msg.setBody(message.toString());
    sendEmail(msg);
  }

  @Override
  public void sendUserRegisteredEmail(OrganizationUser orgUser) {
    List<AbstractOrganizationUser> eligibleOrgAdmins = getOrgUsersByIdAndFilter(orgUser.getOrganization().getId(), (u) -> isEligibleOrgAdmin(u) && u.getSendUserEmail());

    eligibleOrgAdmins.forEach(u -> {
      StringBuilder message = new StringBuilder();
      message.append("<h3>A new user has registered on Sexual Assault Kit Tracking</h3>");
      message.append(orgUser.getDisplayName()).append(" has successfully registered to your organization.");
      message.append(" Please click on the link below to login and verify this user.");
      message.append("<br/><br/><a href=\"").append(uri).append("/organization/").append(orgUser.getOrganization().getId()).append("\">Manage Organization</a>");
      message.append("<br/><br/><a href=\"").append(uri).append("/unsubscribe/sendUserEmail/").append(u.getId()).append("\">Unsubscribe from user e-mail notifications.</a>");

      MailMessage msg = new MailMessage();
      msg.setTo(u.getEmail());
      msg.setSubject(NEW_USER_REGISTERED_SUBJECT);
      msg.setBody(message.toString());
      sendEmail(msg);
    });
  }

  @Override
  public void sendAttorneyNotificationEmail(SexualAssaultKit kit) {
    List<AbstractOrganizationUser> attorneysThatWantNotification = new ArrayList<>();
    getReviewingProsecutorOrganizations(kit).forEach(o -> {
      attorneysThatWantNotification.addAll(getOrgUsersByIdAndFilter(o.getId(), (u) -> isEligibleOrgUser(u) && u.getIncomingKitEmail()));
    });
    attorneysThatWantNotification.forEach(u -> {
      StringBuilder message = new StringBuilder();
      message.append("<h3>Sexual Assault Kit has been updated and needs review.</h3>");
      message.append("Prosecuting attorney oversight is required to confirm the decision made by ");
      message.append(kit.getMedicalDetails().getRequestingLeAgency().getName()).append(" (case #").append(kit.getLeDetails().getCaseNumber()).append(")");
      message.append(" to not submit the sexual assault kit to a lab for analysis. Please use the link below to log in and view the kit details.");
      message.append("<br/><br/><a href=\"").append(uri).append("/attorney/view?id=").append(kit.getId()).append("\">Sexual Assault Kit #").append(kit.getSerialNumber()).append("</a>");
      message.append("<br/><br/><hr/><a href=\"").append(uri).append("/unsubscribe/incomingKitEmail/").append(u.getId()).append("\">Unsubscribe from incoming kit e-mail notifications.</a>");

      MailMessage msg = new MailMessage();
      msg.setTo(u.getEmail());
      msg.setSubject(String.format(ATTORNEY_NOTIFICATION_SUBJECT, kit.getMedicalDetails().getRequestingLeAgency().getName(), kit.getLeDetails().getCaseNumber()));
      msg.setBody(message.toString());
      sendEmail(msg);
    });
  }

  @Override
  public void sendAttorneyHasReviewedNotificationEmail(SexualAssaultKit kit) {
    List<AbstractOrganizationUser> eligibleOrgAdmins = getOrgUsersByIdAndFilter(kit.getCurrentAssignment().getId(), (u) -> isEligibleOrgAdmin(u) && u.getType() == User.Type.LAW_ENFORCEMENT && ((LawEnforcementUser)u).getSendAttorneyReviewedEmail());

    eligibleOrgAdmins.forEach(u -> {
      StringBuilder message = new StringBuilder();
      message.append("<h3>Sexual Assault Kit has been reviewed by the prosecutor.</h3>");
      message.append(kit.getLegalDetails().getReviewingProsecutor()).append(" with ").append(kit.getLegalDetails().getReviewingOrganization().getName());
      message.append(" has reviewed case# ").append(getLeCaseNumberOrAlt(kit, "<i>blank</i>")).append(" and has ");
      if (kit.getLegalDetails().getProsecutorAgrees()) {
        message.append("<b>agreed</b> with the findings.");
      }
      else {
        message.append("<b>disagreed</b> with the findings. Please use the link below to log in and update the Sexual Assault Kit as needed.");
      }
      message.append("<br/><br/><a href=\"").append(uri).append("/law-enforcement/view?id=").append(kit.getId()).append("\">Sexual Assault Kit #").append(kit.getSerialNumber()).append("</a>");
      message.append("<br/><br/><hr/><a href=\"").append(uri).append("/unsubscribe/attorneyReviewedNotificationEmail/").append(u.getId()).append("\">Unsubscribe from attorney review e-mail notifications.</a>");

      MailMessage msg = new MailMessage();
      msg.setTo(u.getEmail());
      msg.setSubject(String.format(ATTORNEY_HAS_REVIEWED_SUBJECT, kit.getSerialNumber(), getLeCaseNumberOrAlt(kit,"")));
      msg.setBody(message.toString());
      sendEmail(msg);
    });
  }

  private String getLeCaseNumberOrAlt(SexualAssaultKit kit, String alt) {
    return StringUtils.isNotBlank(kit.getLeDetails().getCaseNumber()) ? kit.getLeDetails().getCaseNumber() : alt;
  }

  @Override
  public void sendIncomingKitNotification(EventDetails details) {
    List<AbstractOrganizationUser> users = getOrgUsersByIdAndFilter(details.getOrgId(), (u) -> isEligibleOrgUser(u) && u.getIncomingKitEmail());

    users.forEach(u -> {
      StringBuilder message = new StringBuilder();
      message.append("<h3>Incoming Sexual Assault Kits</h3>");
      message.append("The following kit(s) have been marked as sent to your organization:<br/>");
      details.getSerialNumberList().forEach(s -> message.append(s).append("<br/>"));

      message.append("<br/><hr/><a href=\"").append(uri).append("/unsubscribe/incomingKitEmail/").append(u.getId()).append("\">Unsubscribe from incoming kit notifications.</a>");
      MailMessage msg = new MailMessage();
      msg.setTo(u.getEmail());
      msg.setSubject(String.format(LAW_ENFORCEMENT_NOTIFICATION));
      msg.setBody(message.toString());
      sendEmail(msg);
    });
  }

  private String buildLeNotificationMessage(SexualAssaultKit kit) {
    StringBuilder message = new StringBuilder();
    message.append("This is an automated courtesy notification from the Idaho Sexual Assault Kit Tracking System (IKTS). ");
    message.append("Sexual assault kit #").append(kit.getSerialNumber()).append(" is recorded in the system as having been in the possession of ");
    message.append("your law enforcement agency for  <span style=\"color: red;\">").append(MilestoneUtils.getTimeAtLawEnforcement(kit).get()).append("</span> days. ");
    message.append("<b>Section 67-2919 of Idaho Code</b> requires sexual assault kits to be submitted to the Idaho State Police Forensic Services laboratory ");
    message.append("for testing within <span style=\"color: red;\">30</span> days of receipt by a law enforcement agency, unless:");
    message.append("<br/><ul>").append("<li>there is no evidence to support a crime being committed,</li>");
    message.append("<li>it is no longer being investigated as a crime, or</li>");
    message.append("<li>an adult victim expressly indicates that no further forensic examination or testing should occur.</li>").append("</ul><br/>");
    message.append("If sexual assault kit #").append(kit.getSerialNumber()).append(" meets one of the three criteria above, please update the status in ");
    message.append("IKTS immediately so the prosecutor can review the decision to not have the sexual assault kit tested. ");
    message.append("Otherwise, please contact the Idaho State Police Forensic Services laboratory <span style=\"color: red; text-decoration: underline;\">in Meridian</span> ");
    message.append("as soon as possible at 208-884-7170 to arrange for submission of the sexual assault kit #").append(kit.getSerialNumber());
    message.append(" and associated known reference samples. ");
    message.append("<b>If a required known reference sample is unavailable please contact the laboratory at 208-884-7170 prior to submission to discuss the situation.</b>");
    message.append("<br/><br/>If you believe the information in this email is incorrect, or you have further questions regarding this issue, please contact Ms. Nicole Smith at ISPFS ");
    message.append("<a href=\"mailto:nicole.smith@isp.idaho.gov\">nicole.smith@isp.idaho.gov</a> or 208-884-7280");
    return message.toString();
  }

  private List<Organization> getReviewingProsecutorOrganizations(SexualAssaultKit kit) {
    if (kit.getLegalDetails().getReviewingOrganization() != null) {
      return Arrays.asList(kit.getLegalDetails().getReviewingOrganization());
    }
    return organizationRepository.findLegalByJusidictionId(kit.getMedicalDetails().getRequestingLeAgency().getJurisdiction().getId());
  }

  private List<AbstractOrganizationUser> getOrgUsersByIdAndFilter(Long orgId, Predicate<AbstractOrganizationUser> prediate) {
    List<AbstractOrganizationUser> users = organizaitonUserRepository.findByOrganizationIdOrderByDisplayNameAsc(orgId);
    return users.stream().filter(u -> prediate.test(u)).collect(Collectors.toList());
  }

  private boolean isEligibleOrgAdmin(AbstractOrganizationUser user) {
    return isEligibleOrgUser(user) && user.isOrganizationAdmin();
  }

  private boolean isEligibleOrgUser(AbstractOrganizationUser user) {
    return user.isActive() && user.isVerified() && user.isEnabled();
  }

  private boolean sendEmail(MailMessage message) {
    try {
      mailer.send(message);
      if (LOGGER.isDebugEnabled()) {
        LOGGER.debug("Sent email: {}", message);
      }
      return true;
    }
    catch (Exception ex) {
      if (LOGGER.isErrorEnabled()) {
        LOGGER.error("Could not send email to " + message.getToEmails(), ex);
      }
      return false;
    }
  }

  @Autowired
  public void setMailer(Mailer mailer) {
    this.mailer = mailer;
  }

  @Autowired
  public void setOrganizationRepository(OrganizationRepository organizationRepository) {
    this.organizationRepository = organizationRepository;
  }

  @Autowired
  public void setOrganizaitonUserRepository(OrganizationUserRepository organizaitonUserRepository) {
    this.organizaitonUserRepository = organizaitonUserRepository;
  }
}
