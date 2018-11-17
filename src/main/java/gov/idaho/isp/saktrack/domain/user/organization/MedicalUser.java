package gov.idaho.isp.saktrack.domain.user.organization;

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
@DiscriminatorValue(value = "Medical")
@Inheritance(strategy = InheritanceType.SINGLE_TABLE)
@Component @Scope(ConfigurableBeanFactory.SCOPE_PROTOTYPE)
public class MedicalUser extends AbstractOrganizationUser implements Serializable {

  @Override
  public User.Type getType() {
    return User.Type.MEDICAL;
  }

  public void batchReceive(EventDetails eventDetails) {
    userKitService.receive(this, eventDetails);
  }

  public void sendToLawEnforcement(EventDetails eventDetails) {
    if (eventDetails.getSerialNumberList().size() > 1) {
      throw new IllegalTransferException("Only one kit can be sent to Law Enforcement at a time.");
    }
    userKitService.send(this, eventDetails, OrganizationType.LAW_ENFORCEMENT);
  }

  public void sendToLab(EventDetails eventDetails) {
    if (eventDetails.getSerialNumberList().size() > 1) {
      throw new IllegalTransferException("Only one kit can be sent to a lab at a time.");
    }
    userKitService.send(this, eventDetails, OrganizationType.LAB);
  }

  public void batchSendToMedical(EventDetails eventDetails) {
    userKitService.send(this, eventDetails, OrganizationType.MEDICAL);
  }

  public void repurpose(EventDetails eventDetails) {
    if (eventDetails.getSerialNumberList().size() > 1) {
      throw new IllegalTransferException("Only one kit can be repurposed at a time.");
    }

    userKitService.repurpose(this, eventDetails);
  }

  @Override
  public String toString() {
    return "MedicalUser{" + super.toString() + '}';
  }
}