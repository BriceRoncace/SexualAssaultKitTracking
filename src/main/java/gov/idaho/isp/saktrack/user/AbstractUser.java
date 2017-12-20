package gov.idaho.isp.saktrack.user;

import gov.idaho.isp.saktrack.validation.UniqueUsername;
import java.io.Serializable;
import java.util.Collection;
import javax.persistence.DiscriminatorColumn;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Index;
import javax.persistence.Inheritance;
import javax.persistence.InheritanceType;
import javax.persistence.Table;
import javax.persistence.Transient;
import javax.validation.constraints.NotNull;
import org.hibernate.validator.constraints.Email;
import org.hibernate.validator.constraints.NotBlank;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.userdetails.UserDetails;

@Entity
@Table(name = "UserAccount", indexes = {@Index(name = "AK_UserAccount_username", columnList="username", unique = true)})
@DiscriminatorColumn(name = "type")
@Inheritance(strategy = InheritanceType.SINGLE_TABLE)
@UniqueUsername
public abstract class AbstractUser implements User, Serializable {
  @Id
  @GeneratedValue(strategy=GenerationType.AUTO)
  private Long id;

  @NotBlank(message = "{username.blank}")
  private String username;

  @NotBlank(message = "{displayName.blank}")
  private String displayName;

  @NotBlank(message = "{email.blank}")
  @Email(message = "{email.not.valid}")
  private String email;

  @NotBlank(message = "{phone.blank}")
  private String phone;

  @NotNull(message = "{auth.method.null}")
  @Enumerated(EnumType.STRING)
  protected AuthMethod authMethod;

  // Spring security user detail attributes
  @Transient private String password;
  @Transient private Collection<? extends GrantedAuthority> authorities;
  @Transient private boolean accountNonExpired;
  @Transient private boolean accountNonLocked;
  @Transient private boolean credentialsNonExpired;
  @Transient private boolean enabled;

  @Override
  public Long getId() {
    return id;
  }

  public void setId(Long id) {
    this.id = id;
  }

  @Override
  public String getUsername() {
    return username;
  }

  public void setUsername(String username) {
    this.username = username;
  }

  @Override
  public String getDisplayName() {
    return displayName;
  }

  public void setDisplayName(String displayName) {
    this.displayName = displayName;
  }

  @Override
  public String getEmail() {
    return email;
  }

  public void setEmail(String email) {
    this.email = email;
  }

  @Override
  public String getPhone() {
    return phone;
  }

  public void setPhone(String phone) {
    this.phone = phone;
  }

  @Override
  public AuthMethod getAuthMethod() {
    return authMethod;
  }

  public void setAuthMethod(AuthMethod authMethod) {
    this.authMethod = authMethod;
  }

  @Override
  public boolean isAdmin() {
    return this.getType() == Type.ADMIN;
  }

  @Override
  public void setUserDetails(UserDetails userDetails) {
    this.username = userDetails.getUsername();
    this.password = userDetails.getPassword();
    this.authorities = userDetails.getAuthorities();
    this.accountNonExpired = userDetails.isAccountNonExpired();
    this.accountNonLocked = userDetails.isAccountNonLocked();
    this.credentialsNonExpired = userDetails.isCredentialsNonExpired();
    this.enabled = userDetails.isEnabled();
  }

  @Override
  public String getPassword() {
    return password;
  }

  @Override
  public Collection<? extends GrantedAuthority> getAuthorities() {
    return authorities;
  }

  @Override
  public boolean isAccountNonExpired() {
    return accountNonExpired;
  }

  @Override
  public boolean isAccountNonLocked() {
    return accountNonLocked;
  }

  @Override
  public boolean isCredentialsNonExpired() {
    return credentialsNonExpired;
  }

  @Override
  public boolean isEnabled() {
    return enabled;
  }

  @Override
  public String toString() {
    return "AbstractUser{" + "id=" + id + ", username=" + username + ", displayName=" + displayName + ", authMethod=" + authMethod + '}';
  }
}