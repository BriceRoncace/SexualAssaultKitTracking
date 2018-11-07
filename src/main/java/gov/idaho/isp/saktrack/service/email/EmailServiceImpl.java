package gov.idaho.isp.saktrack.service.email;

import gov.idaho.isp.saktrack.domain.SexualAssaultKit;
import gov.idaho.isp.saktrack.domain.dto.EventDetails;
import gov.idaho.isp.saktrack.domain.organization.Organization;
import gov.idaho.isp.saktrack.domain.organization.OrganizationRepository;
import gov.idaho.isp.saktrack.domain.user.User;
import gov.idaho.isp.saktrack.domain.user.organization.AbstractOrganizationUser;
import gov.idaho.isp.saktrack.domain.user.organization.LawEnforcementUser;
import gov.idaho.isp.saktrack.domain.user.organization.OrganizationUser;
import gov.idaho.isp.saktrack.domain.user.organization.OrganizationUserRepository;
import gov.idaho.isp.saktrack.domain.user.password.PasswordToken;
import gov.idaho.isp.saktrack.mailer.MailMessage;
import gov.idaho.isp.saktrack.mailer.Mailer;
import gov.idaho.isp.saktrack.service.TemplateEvaluator;
import gov.idaho.isp.saktrack.util.MilestoneUtils;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

@Service
public class EmailServiceImpl implements EmailService {
  private static final Logger LOGGER = LoggerFactory.getLogger(EmailServiceImpl.class);

  private final String uri;
  private final Mailer mailer;
  private final TemplateEvaluator templateEvaluator;
  private final OrganizationRepository organizationRepository;
  private final OrganizationUserRepository organizaitonUserRepository;

  public EmailServiceImpl(@Value("${application.uri}") String uri, Mailer mailer, TemplateEvaluator templateEvaluator, OrganizationRepository organizationRepository, OrganizationUserRepository organizaitonUserRepository) {
    this.uri = uri;
    this.mailer = mailer;
    this.templateEvaluator = templateEvaluator;
    this.organizationRepository = organizationRepository;
    this.organizaitonUserRepository = organizaitonUserRepository;
  }

  @Override
  public boolean sendPasswordResetEmail(PasswordToken token, User user) {
    Map<String,String> params = new HashMap<>();
    params.put("resetLink", uri + "/reset/password?request=" + token.getToken());
    params.put("username", user.getUsername());
    String text = templateEvaluator.evaluate(EmailTemplate.PASSWORD_RESET, params);

    MailMessage msg = new MailMessage();
    msg.setTo(user.getEmail());
    msg.setSubject(EmailTemplate.PASSWORD_RESET.getSubject());
    msg.setText(text);
    return sendEmail(msg);
  }

  @Override
  public void sendKitsNeedLeActionReminderEmail(SexualAssaultKit kit) {
    Map<String,String> params = new HashMap<>();
    params.put("kitSerialNumber", kit.getSerialNumber());
    params.put("daysAtLawEnforcement", String.valueOf(MilestoneUtils.getTimeAtLawEnforcement(kit).get()));
    String text = templateEvaluator.evaluate(EmailTemplate.SUBMISSION_DUE_WARNING, params);

    MailMessage msg = new MailMessage();
    msg.setTo(getOrgAdminEmailAddresses(kit));
    msg.setSubject(EmailTemplate.SUBMISSION_DUE_WARNING.getSubject());
    msg.setText(text);
    sendEmail(msg);
  }

  @Override
  public void sendKitPastSubmitEmail(SexualAssaultKit kit) {
    Map<String,String> params = new HashMap<>();
    params.put("kitSerialNumber", kit.getSerialNumber());
    params.put("daysAtLawEnforcement", String.valueOf(MilestoneUtils.getTimeAtLawEnforcement(kit).get()));
    String text = templateEvaluator.evaluate(EmailTemplate.SUBMISSION_OVERDUE, params);

    MailMessage msg = new MailMessage();
    msg.setTo(getOrgAdminEmailAddresses(kit));
    msg.setSubject(EmailTemplate.SUBMISSION_OVERDUE.getSubject());
    msg.setText(text);
    sendEmail(msg);
  }

  @Override
  public void sendPlannedDestructionDateReminder(List<SexualAssaultKit> kits) {
    Map<String,String> params = new HashMap<>();
    params.put("link", uri);
    String kitRows = kits.stream().map(kit -> "<tr><td>" + kit.getSerialNumber() + "</td><td>" + kit.getLeDetails().getCaseNumber() + "</td></tr>").collect(Collectors.joining(""));
    params.put("rows", kitRows);
    String text = templateEvaluator.evaluate(EmailTemplate.DESTRUCTION_DATE_NEEDED, params);

    MailMessage msg = new MailMessage();
    msg.setTo(getOrgAdminEmailAddresses(kits.get(0)));
    msg.setSubject(EmailTemplate.DESTRUCTION_DATE_NEEDED.getSubject());
    msg.setText(text);
    sendEmail(msg);
  }

  @Override
  public void sendUpcomingKitDestructionEmail(SexualAssaultKit kit) {
    Map<String,String> params = new HashMap<>();
    params.put("kitSerialNumber", kit.getSerialNumber());
    params.put("kitDestructionDate", kit.getLeDetails().getPlannedDestructionDate().format(DateTimeFormatter.ofPattern("MM/dd/yyyy")));
    String text = templateEvaluator.evaluate(EmailTemplate.DESTRUCTION_DATE_NEEDED, params);

    MailMessage msg = new MailMessage();
    msg.setTo(getOrgAdminEmailAddresses(kit));
    msg.setSubject(String.format(EmailTemplate.DESTRUCTION_REMINDER.getSubject(), kit.getSerialNumber()));
    msg.setText(text);
    sendEmail(msg);
  }

  @Override
  public void sendUserRegisteredEmail(OrganizationUser orgUser) {
    List<AbstractOrganizationUser> eligibleOrgAdmins = getOrgUsersByIdAndFilter(orgUser.getOrganization().getId(), (u) -> isEligibleOrgAdmin(u) && u.getSendUserEmail());

    eligibleOrgAdmins.forEach(u -> {
      Map<String,String> params = new HashMap<>();
      params.put("user", orgUser.getDisplayName());
      params.put("manageOrganizationLink", uri + "/organization/" + u.getOrganization().getId());
      params.put("unsubscribeLink", uri + "/unsubscribe/sendUserEmail/" + u.getId() );
      String text = templateEvaluator.evaluate(EmailTemplate.NEW_USER_REGISTRATION, params);

      MailMessage msg = new MailMessage();
      msg.setTo(u.getEmail());
      msg.setSubject(EmailTemplate.NEW_USER_REGISTRATION.getSubject());
      msg.setText(text);
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
      Map<String,String> params = new HashMap<>();
      params.put("kitSerialNumber", kit.getSerialNumber());
      params.put("lawEnforcementAgencyName", kit.getMedicalDetails().getRequestingLeAgency().getName());
      params.put("leCaseNumber", kit.getLeDetails().getCaseNumber());
      params.put("viewLink", uri + "/attorney/view?id=" + kit.getId());
      params.put("unsubscribeLink", uri + "/unsubscribe/incomingKitEmail/" + u.getId() );
      String text = templateEvaluator.evaluate(EmailTemplate.ATTORNEY_NOTIFICATION, params);

      MailMessage msg = new MailMessage();
      msg.setTo(u.getEmail());
      msg.setSubject(String.format(EmailTemplate.ATTORNEY_NOTIFICATION.getSubject(), kit.getMedicalDetails().getRequestingLeAgency().getName(), kit.getLeDetails().getCaseNumber()));
      msg.setText(text);
      sendEmail(msg);
    });
  }

  @Override
  public void sendAttorneyHasReviewedNotificationEmail(SexualAssaultKit kit) {
    List<AbstractOrganizationUser> eligibleOrgAdmins = getOrgUsersByIdAndFilter(kit.getCurrentAssignment().getId(), (u) -> isEligibleOrgAdmin(u) && u.getType() == User.Type.LAW_ENFORCEMENT && ((LawEnforcementUser)u).getSendAttorneyReviewedEmail());

    eligibleOrgAdmins.forEach(u -> {
      Map<String,String> params = new HashMap<>();
      params.put("reviewedBy", kit.getLegalDetails().getReviewingProsecutor());
      params.put("reviewingOrganization", kit.getLegalDetails().getReviewingOrganization().getName());
      params.put("leCaseNumber", getLeCaseNumberOrAlt(kit, "<i>blank</i>"));
      if (kit.getLegalDetails().getProsecutorAgrees()) {
        params.put("decision", "<b>agreed</b> with the findings.");
      }
      else {
        params.put("decision", "<b>disagreed</b> with the findings. Please use the link below to log in and update the Sexual Assault Kit as needed.");
      }
      params.put("kitSerialNumber", kit.getSerialNumber());
      params.put("viewLink", uri + "/law-enforcement/view?id=" + kit.getId());
      params.put("unsubscribeLink", uri + "/unsubscribe/attorneyReviewedNotificationEmail/" + u.getId() );
      String text = templateEvaluator.evaluate(EmailTemplate.ATTORNEY_REVIEW_COMPLETE, params);

      MailMessage msg = new MailMessage();
      msg.setTo(u.getEmail());
      msg.setSubject(String.format(EmailTemplate.ATTORNEY_REVIEW_COMPLETE.getSubject(), kit.getSerialNumber(), getLeCaseNumberOrAlt(kit,"")));
      msg.setText(text);
      sendEmail(msg);
    });
  }

  @Override
  public void sendIncomingKitNotification(EventDetails details) {
    List<AbstractOrganizationUser> users = getOrgUsersByIdAndFilter(details.getOrgId(), (u) -> isEligibleOrgUser(u) && u.getIncomingKitEmail());

    users.forEach(u -> {
      Map<String,String> params = new HashMap<>();
      params.put("kits", details.getSerialNumberList().stream().collect(Collectors.joining("<br>")));
      params.put("viewLink", uri);
      params.put("unsubscribeLink", uri + "/unsubscribe/incomingKitEmail/" + u.getId() );
      String text = templateEvaluator.evaluate(EmailTemplate.LAW_ENFORCEMENT_INCOMING_KIT, params);

      MailMessage msg = new MailMessage();
      msg.setTo(u.getEmail());
      msg.setSubject(EmailTemplate.LAW_ENFORCEMENT_INCOMING_KIT.getSubject());
      msg.setText(text);
      sendEmail(msg);
    });
  }

  private String getLeCaseNumberOrAlt(SexualAssaultKit kit, String alt) {
    return StringUtils.isNotBlank(kit.getLeDetails().getCaseNumber()) ? kit.getLeDetails().getCaseNumber() : alt;
  }


  private List<String> getOrgAdminEmailAddresses(SexualAssaultKit kit) {
    List<AbstractOrganizationUser> adminUsers = getOrgUsersByIdAndFilter(kit.getMedicalDetails().getRequestingLeAgency().getId(), (u) -> isEligibleOrgAdmin(u));
    List<String> addresses = adminUsers.stream().map(User::getEmail).collect(Collectors.toList());
    return addresses;
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
}