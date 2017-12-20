package gov.idaho.isp.saktrack.user.organization;

import gov.idaho.isp.saktrack.SexualAssaultKit;
import gov.idaho.isp.saktrack.dto.EventDetails;
import gov.idaho.isp.saktrack.exception.IllegalTransferException;
import gov.idaho.isp.saktrack.organization.OrganizationType;
import gov.idaho.isp.saktrack.user.User;
import java.io.Serializable;
import javax.persistence.DiscriminatorValue;
import javax.persistence.Entity;
import javax.persistence.Inheritance;
import javax.persistence.InheritanceType;
import org.springframework.beans.factory.config.ConfigurableBeanFactory;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Component;

@Entity
@DiscriminatorValue(value = "Law Enforcement")
@Inheritance(strategy = InheritanceType.SINGLE_TABLE)
@Component @Scope(ConfigurableBeanFactory.SCOPE_PROTOTYPE)
public class LawEnforcementUser extends AbstractOrganizationUser implements Serializable {

  private boolean sendAttorneyReviewedEmail;

  public boolean getSendAttorneyReviewedEmail() {
    return sendAttorneyReviewedEmail;
  }

  public void setSendAttorneyReviewedEmail(boolean sendAttorneyReviewedEmail) {
    this.sendAttorneyReviewedEmail = sendAttorneyReviewedEmail;
  }

  @Override
  public User.Type getType() {
    return User.Type.LAW_ENFORCEMENT;
  }

  public void batchReceive(EventDetails eventDetails) {
    userKitService.receive(this, eventDetails);
  }

  public void releaseForProsecutorReview(SexualAssaultKit kit) {
    userKitService.releaseForProsecutorReview(this, kit);
  }

  public void sendToLawEnforcement(EventDetails eventDetails) {
    if (eventDetails.getSerialNumberList().size() > 1) {
      throw new IllegalTransferException("Only one kit can be sent to Law Enforcement at a time.");
    }

    userKitService.send(this, eventDetails, OrganizationType.LAW_ENFORCEMENT);
  }

  public void sendToLab(EventDetails eventDetails) {
    if (eventDetails.getSerialNumberList().size() > 1) {
      throw new IllegalTransferException("Only one kit can be sent to the Lab at a time.");
    }

    userKitService.send(this, eventDetails, OrganizationType.LAB);
  }

  public void sendToMedical(EventDetails eventDetails) {
    if (eventDetails.getSerialNumberList().size() > 1) {
      throw new IllegalTransferException("Only one kit can be sent to Medical at a time.");
    }

    userKitService.send(this, eventDetails, OrganizationType.MEDICAL);
  }

  public void destroy(EventDetails eventDetails) {
    if (eventDetails.getSerialNumberList().size() > 1) {
      throw new IllegalTransferException("Only one kit can be destroyed at a time.");
    }
    userKitService.destroy(this, eventDetails);
  }

  public boolean isRequestingAgencyMyAgency(SexualAssaultKit kit) {
    return kit.getMedicalDetails() != null
      && kit.getMedicalDetails().getRequestingLeAgency() != null
      && kit.getMedicalDetails().getRequestingLeAgency().getId() != null
      && kit.getMedicalDetails().getRequestingLeAgency().getId().equals(this.getOrganization().getId());
  }

  @Override
  public String toString() {
    return "LawEnforcementUser{" + super.toString() + '}';
  }
}
