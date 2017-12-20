package gov.idaho.isp.saktrack;

import gov.idaho.isp.saktrack.organization.Organization;
import gov.idaho.isp.saktrack.persistence.listener.LastModifiedEntityListener;
import gov.idaho.isp.saktrack.persistence.listener.SexualAssaultKitAware;
import gov.idaho.isp.saktrack.validation.LocalDatePast;
import gov.idaho.isp.saktrack.validation.group.SendKit;
import java.io.Serializable;
import java.time.LocalDate;
import javax.persistence.Entity;
import javax.persistence.EntityListeners;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToOne;
import javax.validation.constraints.NotNull;

@Entity
@EntityListeners(LastModifiedEntityListener.class)
public class MedicalDetails implements SexualAssaultKitAware, Serializable {
  public enum VictimType implements HasLabel {
    NAMED("Named"),
    ANONYMOUS("Anonymous");

    private final String label;

    private VictimType(String label) {
      this.label = label;
    }

    @Override
    public String getLabel() {
      return label;
    }
  }

  @Id
  @GeneratedValue(strategy=GenerationType.AUTO)
  private Long id;

  @LocalDatePast(message = "{collectionDate.in.future}")
  @NotNull(message = "{collectionDate.null}", groups = {SendKit.class})
  private LocalDate collectionDate;

  @Enumerated(EnumType.STRING)
  @NotNull(message = "{victimType.null}", groups = {SendKit.class})
  private VictimType victimType;

  @ManyToOne
  @JoinColumn(name = "requestingOrganizationId")
  @NotNull(message = "{org.id.null}", groups = {SendKit.class})
  private Organization requestingLeAgency;

  @OneToOne(mappedBy = "medicalDetails")
  private SexualAssaultKit sexualAssaultKit;

  public boolean isComplete() {
    return (victimType != null && requestingLeAgency != null && collectionDate != null);
  }

  public boolean isEmpty() {
    return (victimType == null && requestingLeAgency == null && collectionDate == null);
  }

  public Long getId() {
    return id;
  }

  public void setId(Long id) {
    this.id = id;
  }

  public LocalDate getCollectionDate() {
    return collectionDate;
  }

  public void setCollectionDate(LocalDate collectionDate) {
    this.collectionDate = collectionDate;
  }

  public VictimType getVictimType() {
    return victimType;
  }

  public void setVictimType(VictimType victimType) {
    this.victimType = victimType;
  }

  public Organization getRequestingLeAgency() {
    return requestingLeAgency;
  }

  public void setRequestingLeAgency(Organization requestingLeAgency) {
    this.requestingLeAgency = requestingLeAgency;
  }

  @Override
  public SexualAssaultKit getSexualAssaultKit() {
    return sexualAssaultKit;
  }

  public void setSexualAssaultKit(SexualAssaultKit sexualAssaultKit) {
    this.sexualAssaultKit = sexualAssaultKit;
  }

  @Override
  public String toString() {
    return "MedicalDetails{" + "id=" + id + ", victimType=" + victimType + ", requestingLeAgency=" + requestingLeAgency + '}';
  }
}