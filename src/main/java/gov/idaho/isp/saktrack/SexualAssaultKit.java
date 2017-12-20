package gov.idaho.isp.saktrack;

import gov.idaho.isp.saktrack.jurisdiction.Jurisdiction;
import gov.idaho.isp.saktrack.organization.Organization;
import gov.idaho.isp.saktrack.persistence.listener.LastModifiedEntityListener;
import gov.idaho.isp.saktrack.persistence.listener.SexualAssaultKitAware;
import gov.idaho.isp.saktrack.util.EventUtil;
import gov.idaho.isp.saktrack.util.MilestoneUtils;
import gov.idaho.isp.saktrack.validation.ReviewingProsecutorOrganizationSetIfRequired;
import java.io.Serializable;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import javax.persistence.CascadeType;
import javax.persistence.Entity;
import javax.persistence.EntityListeners;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Index;
import javax.persistence.JoinColumn;
import javax.persistence.NamedAttributeNode;
import javax.persistence.NamedEntityGraph;
import javax.persistence.NamedQueries;
import javax.persistence.NamedQuery;
import javax.persistence.OneToMany;
import javax.persistence.OneToOne;
import javax.persistence.OrderColumn;
import javax.persistence.Table;
import javax.validation.Valid;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Pattern;

@Entity
@EntityListeners(LastModifiedEntityListener.class)
@Table(indexes = {@Index(name = "AK_SexualAssaultKit_serialNumber", columnList="serialNumber", unique = true)})
@NamedQueries({
  @NamedQuery(name = "SexualAssaultKit.findByCurrentAssignmentId", query = "from SexualAssaultKit where currentAssignment.id = ?1 order by serialNumber")
})
@NamedEntityGraph(name = "sak.with.events", attributeNodes = @NamedAttributeNode("chainOfCustody"))
@ReviewingProsecutorOrganizationSetIfRequired
public class SexualAssaultKit implements SexualAssaultKitAware, Serializable {

  @Id
  @GeneratedValue(strategy=GenerationType.AUTO)
  private Long id;

  @NotNull(message = "{serialNumber.null}")
  @Pattern(regexp = "\\d+", message = "{serialNumber.not.number}")
  private String serialNumber;

  @NotNull(message = "{expirationDate.null}")
  private LocalDate expirationDate;

  private LocalDateTime lastModified;

  private String lastModifiedBy;

  @OneToMany(cascade = CascadeType.ALL, orphanRemoval = true, fetch = FetchType.EAGER, mappedBy = "sexualAssaultKit")
  @Valid
  @OrderColumn(name = "sortOrder")
  private final List<ChainOfCustodyEvent> chainOfCustody = new ArrayList<>();

  private boolean questionableEvents;

  @OneToOne(cascade = CascadeType.ALL, orphanRemoval = true)
  @JoinColumn(name = "medicalDetailsId")
  @Valid
  private MedicalDetails medicalDetails = new MedicalDetails();

  @OneToOne(cascade = CascadeType.ALL, orphanRemoval = true)
  @JoinColumn(name = "leDetailsId")
  @Valid
  private LawEnforcementDetails leDetails = new LawEnforcementDetails();

  @OneToOne(cascade = CascadeType.ALL, orphanRemoval = true)
  @JoinColumn(name = "legalDetailsId")
  @Valid
  private LegalDetails legalDetails = new LegalDetails();

  @OneToOne(cascade = CascadeType.ALL, orphanRemoval = true)
  @JoinColumn(name = "labDetailsId")
  @Valid
  private LabDetails labDetails = new LabDetails();

  @OneToOne
  @JoinColumn(name = "organizationId")
  private Organization currentAssignment;

  public SexualAssaultKit() {
  }

  public SexualAssaultKit(String serialNumber) {
    this.serialNumber = serialNumber;
  }

  public ChainOfCustodyEvent getCurrentCustody() {
    return !chainOfCustody.isEmpty() ? chainOfCustody.get(chainOfCustody.size()-1) : null;
  }

  public void addChainOfCustodyEvent(ChainOfCustodyEvent event) {
    event.setSexualAssaultKit(this);
    chainOfCustody.add(event);
  }

  public KitStatus getStatus() {
    return KitStatus.getStatus(this);
  }

  public Jurisdiction getJurisdiction() {
    return currentAssignment.getJurisdiction();
  }

  public Long getId() {
    return id;
  }

  public void setId(Long id) {
    this.id = id;
  }

  public String getSerialNumber() {
    return serialNumber;
  }

  public void setSerialNumber(String serialNumber) {
    this.serialNumber = serialNumber;
  }

  public LocalDate getExpirationDate() {
    return expirationDate;
  }

  public void setExpirationDate(LocalDate expirationDate) {
    this.expirationDate = expirationDate;
  }

  public LocalDateTime getLastModified() {
    return lastModified;
  }

  public void setLastModified(LocalDateTime lastModified) {
    this.lastModified = lastModified;
  }

  public String getLastModifiedBy() {
    return lastModifiedBy;
  }

  public void setLastModifiedBy(String lastModifiedBy) {
    this.lastModifiedBy = lastModifiedBy;
  }

  public List<ChainOfCustodyEvent> getChainOfCustody() {
    return chainOfCustody;
  }

  public boolean getQuestionableEvents() {
    return questionableEvents;
  }

  public void setQuestionableEvents(boolean questionableEvents) {
    this.questionableEvents = questionableEvents;
  }

  public boolean hasCalculatedMissingEvents() {
    return EventUtil.hasMissingSendEvents(chainOfCustody);
  }

  public MedicalDetails getMedicalDetails() {
    if (medicalDetails == null) {
      medicalDetails = new MedicalDetails();
    }
    return medicalDetails;
  }

  public void setMedicalDetails(MedicalDetails medicalDetails) {
    this.medicalDetails = medicalDetails;
  }

  public LawEnforcementDetails getLeDetails() {
    if (leDetails == null) {
      leDetails = new LawEnforcementDetails();
    }
    return leDetails;
  }

  public void setLeDetails(LawEnforcementDetails leDetails) {
    this.leDetails = leDetails;
  }

  public LegalDetails getLegalDetails() {
    if (legalDetails == null) {
      legalDetails = new LegalDetails();
    }
    return legalDetails;
  }

  public void setLegalDetails(LegalDetails legalDetails) {
    this.legalDetails = legalDetails;
  }

  public LabDetails getLabDetails() {
    if (labDetails == null) {
      labDetails = new LabDetails();
    }
    return labDetails;
  }

  public void setLabDetails(LabDetails labDetails) {
    this.labDetails = labDetails;
  }

  public Organization getCurrentAssignment() {
    return currentAssignment;
  }

  public void setCurrentAssignment(Organization currentAssignment) {
    this.currentAssignment = currentAssignment;
  }

  @Override
  public SexualAssaultKit getSexualAssaultKit() {
    return this;
  }

  public boolean isDestroyable() {
    KitStatus status = getStatus();
    return leDetails != null &&
      leDetails.getPlannedDestructionDate() != null &&
      !leDetails.getPlannedDestructionDate().isAfter(LocalDate.now()) &&
      (status == KitStatus.ANALYZED || status == KitStatus.WILL_NOT_BE_ANALYZED);
  }

  public Long getTimeAtLe() {
    return MilestoneUtils.getTimeAtLawEnforcement(this).orElse(null);
  }

  @Override
  public String toString() {
    return "SexualAssaultKit{" + "id=" + id + ", serialNumber=" + serialNumber + ", chainOfCustody=" + chainOfCustody + ", medicalDetails=" + medicalDetails + ", leDetails=" + leDetails + ", labDetails=" + labDetails + ", currentAssignment=" + currentAssignment + '}';
  }
}
