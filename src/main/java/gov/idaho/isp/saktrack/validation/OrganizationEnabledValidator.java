package gov.idaho.isp.saktrack.validation;

import gov.idaho.isp.saktrack.organization.Organization;
import gov.idaho.isp.saktrack.organization.OrganizationRepository;
import javax.validation.ConstraintValidator;
import javax.validation.ConstraintValidatorContext;
import org.springframework.beans.factory.annotation.Autowired;

public class OrganizationEnabledValidator implements ConstraintValidator<OrganizationEnabled, Long> {
  private OrganizationRepository organizationRepository;

  @Override
  public void initialize(OrganizationEnabled a) {
  }

  @Override
  public boolean isValid(Long orgId, ConstraintValidatorContext cvc) {
    if (organizationRepository != null) {
      Organization org = organizationRepository.findOne(orgId);
      if (org != null) {
        return org.getEnabled();
      }
      return false;
    }
    return true;
  }

  @Autowired
  public void setOrganizationRepository(OrganizationRepository organizationRepository) {
    this.organizationRepository = organizationRepository;
  }
}
