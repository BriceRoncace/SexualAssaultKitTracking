package gov.idaho.isp.saktrack.user.password;

import java.io.Serializable;
import java.time.LocalDateTime;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Index;
import javax.persistence.Table;

@Entity
@Table(indexes = {@Index(name = "AK_PasswordToken_token", columnList="token", unique = true)})
public class PasswordToken implements Serializable {
  @Id
  @GeneratedValue(strategy=GenerationType.AUTO)
  private Long id;

  private String token;

  private LocalDateTime generationDate;

  private Long userId;

  public PasswordToken() {}

  public PasswordToken(String token, Long userId) {
    this.token = token;
    this.userId = userId;
    this.generationDate = LocalDateTime.now();
  }

  public Long getId() {
    return id;
  }

  public void setId(Long id) {
    this.id = id;
  }

  public String getToken() {
    return token;
  }

  public void setToken(String token) {
    this.token = token;
  }

  public LocalDateTime getGenerationDate() {
    return generationDate;
  }

  public void setGenerationDate(LocalDateTime generationDate) {
    this.generationDate = generationDate;
  }

  public Long getUserId() {
    return userId;
  }

  public void setUserId(Long userId) {
    this.userId = userId;
  }
}
