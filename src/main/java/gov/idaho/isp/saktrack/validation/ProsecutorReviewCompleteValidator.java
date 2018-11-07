package gov.idaho.isp.saktrack.validation;

import gov.idaho.isp.saktrack.domain.KitStatus;
import gov.idaho.isp.saktrack.domain.LawEnforcementDetails;
import javax.validation.ConstraintValidator;
import javax.validation.ConstraintValidatorContext;

public class ProsecutorReviewCompleteValidator implements ConstraintValidator<ProsecutorReviewComplete, LawEnforcementDetails> {
  @Override
  public void initialize(ProsecutorReviewComplete constraintAnnotation) {
  }

  @Override
  public boolean isValid(LawEnforcementDetails leDetails, ConstraintValidatorContext context) {
    if (leDetails == null || leDetails.getSexualAssaultKit() == null) {
      return true;
    }

    return leDetails.getSexualAssaultKit().getStatus() != KitStatus.AWAITING_LEGAL_REVIEW;
  }
}