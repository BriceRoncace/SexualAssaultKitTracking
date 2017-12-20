package gov.idaho.isp.saktrack.validation;

import gov.idaho.isp.saktrack.SexualAssaultKit;
import javax.validation.ConstraintValidator;
import javax.validation.ConstraintValidatorContext;

public class ReviewingProsecutorOrganizationSetIfRequiredValidator implements ConstraintValidator<ReviewingProsecutorOrganizationSetIfRequired, SexualAssaultKit> {
  @Override
  public void initialize(ReviewingProsecutorOrganizationSetIfRequired constraintAnnotation) {
  }

  @Override
  public boolean isValid(SexualAssaultKit kit, ConstraintValidatorContext context) {
    if (kit == null || kit.getMedicalDetails() == null || kit.getMedicalDetails().getRequestingLeAgency() == null || kit.getLeDetails() == null) {
      return true;
    }

    if (kit.getMedicalDetails().getRequestingLeAgency().isStatewide() && Boolean.FALSE.equals(kit.getLeDetails().getMeetsSubmissionCriteria())) {
      return kit.getLegalDetails().getReviewingOrganization() != null;
    }

    return true;
  }

}
