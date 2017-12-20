package gov.idaho.isp.saktrack.validation;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import javax.validation.Constraint;
import javax.validation.Payload;


@Constraint(validatedBy = RangeValidator.class)
@Target( {ElementType.METHOD, ElementType.FIELD })
@Retention(RetentionPolicy.RUNTIME)
public @interface Range {
  public String message() default "{invalid.range}";
  public Class<?>[] groups() default {};
  public Class<? extends Payload>[] payload() default {};
}