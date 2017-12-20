package gov.idaho.isp.saktrack;

import gov.idaho.isp.saktrack.LawEnforcementDetails.NonSubmissionReason;
import gov.idaho.isp.saktrack.organization.Organization;
import gov.idaho.isp.saktrack.persistence.listener.LastModifiedEntityListener;
import gov.idaho.isp.saktrack.persistence.listener.SexualAssaultKitAware;
import gov.idaho.isp.saktrack.validation.LocalDatePast;
import java.io.Serializable;
import java.time.LocalDate;
import javax.persistence.CascadeType;
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

@Entity
@EntityListeners(LastModifiedEntityListener.class)
public class LegalDetails implements SexualAssaultKitAware, Serializable {
  @Id
  @GeneratedValue(strategy=GenerationType.AUTO)
  private Long id;

  @JoinColumn(name = "releasedForProsecutorReview")
  private LocalDate releasedForReview;

  @Enumerated(EnumType.STRING)
  private NonSubmissionReason nonSubmissionReason;

  @ManyToOne(cascade = CascadeType.ALL)
  @JoinColumn(name = "reviewingProsecutorOrgId")
  private Organization reviewingOrganization;

  private String reviewingProsecutor;

  private Boolean prosecutorAgrees;

  private String prosecutorNotes;

  @LocalDatePast(message = "{reviewFinalized.in.future}")
  private LocalDate reviewFinalized;

  @OneToOne(mappedBy = "legalDetails")
  private SexualAssaultKit sexualAssaultKit;

  public Long getId() {
    return id;
  }

  public void setId(Long id) {
    this.id = id;
  }

  public LocalDate getReleasedForReview() {
    return releasedForReview;
  }

  public void setReleasedForReview(LocalDate releasedForReview) {
    this.releasedForReview = releasedForReview;
  }

  public NonSubmissionReason getNonSubmissionReason() {
    return nonSubmissionReason;
  }

  public void setNonSubmissionReason(NonSubmissionReason nonSubmissionReason) {
    this.nonSubmissionReason = nonSubmissionReason;
  }

  public Organization getReviewingOrganization() {
    return reviewingOrganization;
  }

  public void setReviewingOrganization(Organization reviewingOrganization) {
    this.reviewingOrganization = reviewingOrganization;
  }

  public Boolean getProsecutorAgrees() {
    return prosecutorAgrees;
  }

  public void setProsecutorAgrees(Boolean prosecutorAgrees) {
    this.prosecutorAgrees = prosecutorAgrees;
  }

  public String getProsecutorNotes() {
    return prosecutorNotes;
  }

  public void setProsecutorNotes(String prosecutorNotes) {
    this.prosecutorNotes = prosecutorNotes;
  }

  public String getReviewingProsecutor() {
    return reviewingProsecutor;
  }

  public void setReviewingProsecutor(String reviewingProsecutor) {
    this.reviewingProsecutor = reviewingProsecutor;
  }

  public LocalDate getReviewFinalized() {
    return reviewFinalized;
  }

  public void setReviewFinalized(LocalDate reviewFinalized) {
    this.reviewFinalized = reviewFinalized;
  }

  @Override
  public SexualAssaultKit getSexualAssaultKit() {
    return sexualAssaultKit;
  }

  public void setSexualAssaultKit(SexualAssaultKit sexualAssaultKit) {
    this.sexualAssaultKit = sexualAssaultKit;
  }
}