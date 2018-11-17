package gov.idaho.isp.saktrack.domain.user.organization;

import gov.idaho.isp.saktrack.domain.dto.CreateKitEventDetails;
import gov.idaho.isp.saktrack.domain.dto.EventDetails;
import gov.idaho.isp.saktrack.exception.IllegalTransferException;
import gov.idaho.isp.saktrack.domain.organization.OrganizationType;
import gov.idaho.isp.saktrack.domain.user.User;
import java.io.Serializable;
import javax.persistence.DiscriminatorValue;
import javax.persistence.Entity;
import javax.persistence.Inheritance;
import javax.persistence.InheritanceType;
import org.springframework.beans.factory.config.ConfigurableBeanFactory;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Component;

@Entity
@DiscriminatorValue(value = "Lab")
@Inheritance(strategy = InheritanceType.SINGLE_TABLE)
@Component @Scope(ConfigurableBeanFactory.SCOPE_PROTOTYPE)
public class LabUser extends AbstractOrganizationUser implements Serializable {

  @Override
  public User.Type getType() {
    return User.Type.LAB;
  }

  public void batchCreate(CreateKitEventDetails eventDetails) {
    userKitService.create(this, eventDetails);
  }

  public void batchDelete(EventDetails eventDetails) {
    userKitService.delete(this, eventDetails);
  }

  public void receive(EventDetails eventDetails) {
    if (eventDetails.getSerialNumberList().size() > 1) {
      throw new IllegalTransferException("Only one kit can be received at a time.");
    }
    userKitService.receive(this, eventDetails);
  }

  public void batchSendToMedical(EventDetails eventDetails) {
    userKitService.send(this, eventDetails, OrganizationType.MEDICAL);
  }

  public void batchSendToLab(EventDetails eventDetails) {
    userKitService.send(this, eventDetails, OrganizationType.LAB);
  }

  public void batchSendToLawEnforcement(EventDetails eventDetails) {
    userKitService.send(this, eventDetails, OrganizationType.LAW_ENFORCEMENT);
  }

  public void returnToLawEnforcement(EventDetails eventDetails) {
    if (eventDetails.getSerialNumberList().size() > 1) {
      throw new IllegalTransferException("Only one kit can be returned to Law Enforcement at a time.");
    }

    userKitService.send(this, eventDetails, OrganizationType.LAW_ENFORCEMENT);
  }

  public void repurpose(EventDetails eventDetails) {
    if (eventDetails.getSerialNumberList().size() > 1) {
      throw new IllegalTransferException("Only one kit can be repurposed at a time.");
    }

    userKitService.repurpose(this, eventDetails);
  }

  @Override
  public String toString() {
    return "LabUser{" + super.toString() + '}';
  }
}