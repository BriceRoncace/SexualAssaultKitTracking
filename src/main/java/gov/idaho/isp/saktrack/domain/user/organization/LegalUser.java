package gov.idaho.isp.saktrack.domain.user.organization;

import gov.idaho.isp.saktrack.domain.SexualAssaultKit;
import javax.persistence.DiscriminatorValue;
import javax.persistence.Entity;
import javax.persistence.Inheritance;
import javax.persistence.InheritanceType;
import org.springframework.beans.factory.config.ConfigurableBeanFactory;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Component;

@Entity
@DiscriminatorValue(value = "Legal")
@Inheritance(strategy = InheritanceType.SINGLE_TABLE)
@Component @Scope(ConfigurableBeanFactory.SCOPE_PROTOTYPE)
public class LegalUser extends AbstractOrganizationUser {

  @Override
  public Type getType() {
    return Type.LEGAL;
  }

  public void agreeWithNonSubmission(SexualAssaultKit kit, String notes) {
    this.userKitService.review(this, kit, notes, true);
  }

  public void disagreeWithNonSubmission(SexualAssaultKit kit, String notes) {
    this.userKitService.review(this, kit, notes, false);
  }
}