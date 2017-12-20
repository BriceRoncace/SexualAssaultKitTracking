package gov.idaho.isp.saktrack.validation;

import gov.idaho.isp.saktrack.KitStatus;
import gov.idaho.isp.saktrack.LawEnforcementDetails;
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