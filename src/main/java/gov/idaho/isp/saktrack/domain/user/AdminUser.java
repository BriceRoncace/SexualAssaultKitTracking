package gov.idaho.isp.saktrack.domain.user;

import java.io.Serializable;
import javax.persistence.DiscriminatorValue;
import javax.persistence.Entity;
import javax.persistence.Inheritance;
import javax.persistence.InheritanceType;

@Entity
@DiscriminatorValue(value = "Admin")
@Inheritance(strategy = InheritanceType.SINGLE_TABLE)
public class AdminUser extends AbstractUser implements User, Serializable {

  @Override
  public User.Type getType() {
    return User.Type.ADMIN;
  }

  @Override
  public boolean isOrganizationAdmin() {
    return true;
  }
}