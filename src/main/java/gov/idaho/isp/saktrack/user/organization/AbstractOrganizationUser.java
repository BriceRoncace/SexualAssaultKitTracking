package gov.idaho.isp.saktrack.user.organization;

import gov.idaho.isp.saktrack.organization.Organization;
import gov.idaho.isp.saktrack.user.AbstractUser;
import gov.idaho.isp.saktrack.validation.PropertiesEqual;
import java.time.LocalDate;
import javax.persistence.Entity;
import javax.persistence.Inheritance;
import javax.persistence.InheritanceType;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.NamedQuery;
import javax.persistence.Transient;
import javax.validation.constraints.NotNull;
import org.hibernate.validator.constraints.NotBlank;
import org.springframework.beans.factory.annotation.Autowired;

@Entity
@NamedQuery(name = "AbstractOrganizationUser.findUnverifiedUserByOrganization", query = "from AbstractOrganizationUser where organization.id = ?1 and verifiedDate = null")
@Inheritance(strategy = InheritanceType.SINGLE_TABLE)
@PropertiesEqual(propertyNameOne = "passkey", propertyNameTwo = "organization.passkey", message = "{passkey.not.valid}")
public abstract class AbstractOrganizationUser extends AbstractUser implements OrganizationUser {
  private String password;

  @NotNull(message = "{user.organization.null}")
  @JoinColumn(name = "organizationId")
  @ManyToOne
  private Organization organization;

  private boolean organizationAdmin;

  private boolean organizationContact;

  @NotBlank(message = "{passkey.blank}")
  private String passkey;

  private boolean enabled;

  private LocalDate verifiedDate;

  private boolean sendUserEmail;

  private boolean incomingKitEmail;

  @Transient
  protected transient UserKitService userKitService;

  @Override
  public boolean isActive() {
    return enabled && organization != null && organization.getEnabled();
  }

  @Override
  public boolean isVerified() {
    return verifiedDate != null;
  }

  @Override
  public String getPassword() {
    return password;
  }

  public void setPassword(String password) {
    this.password = password;
  }

  @Override
  public Organization getOrganization() {
    return organization;
  }

  public void setOrganization(Organization organization) {
    this.organization = organization;
  }

  @Override
  public boolean isOrganizationAdmin() {
    return organizationAdmin;
  }

  public void setOrganizationAdmin(boolean organizationAdmin) {
    this.organizationAdmin = organizationAdmin;
  }

  @Override
  public boolean isOrganizationContact() {
    return organizationContact;
  }

  public void setOrganizationContact(boolean organizationContact) {
    this.organizationContact = organizationContact;
  }

  @Override
  public String getPasskey() {
    return passkey;
  }

  public void setPasskey(String passkey) {
    this.passkey = passkey;
  }

  @Override
  public boolean isEnabled() {
    return enabled;
  }

  public void setEnabled(boolean enabled) {
    this.enabled = enabled;
  }

  public LocalDate getVerifiedDate() {
    return verifiedDate;
  }

  public void setVerifiedDate(LocalDate verifiedDate) {
    this.verifiedDate = verifiedDate;
  }

  public boolean getSendUserEmail() {
    return sendUserEmail;
  }

  public void setSendUserEmail(boolean sendUserEmail) {
    this.sendUserEmail = sendUserEmail;
  }

  public boolean getIncomingKitEmail() {
    return incomingKitEmail;
  }

  public void setIncomingKitEmail(boolean incomingKitEmail) {
    this.incomingKitEmail = incomingKitEmail;
  }

  @Autowired
  void setUserKitService(UserKitService userKitService) {
    this.userKitService = userKitService;
  }

  public boolean hasDependencies() {
    return userKitService != null;
  }

  @Override
  public String toString() {
    return "AbstractOrganizationUser{" + "enabled=" + enabled + ", password=" + password + ", organization=" + organization + ", organizationAdmin=" + organizationAdmin + ", passkey=" + passkey + ", service=" + userKitService + ", " + super.toString() + '}';
  }
}
