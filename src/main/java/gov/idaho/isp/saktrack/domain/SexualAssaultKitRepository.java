package gov.idaho.isp.saktrack.domain;

import java.time.LocalDate;
import java.util.List;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.EntityGraph;
import org.springframework.data.jpa.repository.EntityGraph.EntityGraphType;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Query;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

public interface SexualAssaultKitRepository extends SexualAssaultKitRepositoryPlus, JpaRepository<SexualAssaultKit,Long>, JpaSpecificationExecutor<SexualAssaultKit> {
  List<SexualAssaultKit> findByCurrentAssignmentId(Long id);
  List<SexualAssaultKit> findByQuestionableEventsTrue();
  List<SexualAssaultKit> findByLeDetailsPlannedDestructionDate(LocalDate plannedDestructionDate);
  Page<SexualAssaultKit> findBySerialNumberOrLabDetailsCaseNumber(String serialNumber, String caseNumber, Pageable pageable);

  @Query(value = "select kit from SexualAssaultKit kit "
    + "join kit.chainOfCustody event "
    + "where event.eventFlag = 'LE_RECEIVED_COLLECTED_KIT' and event.eventDate = ?1 "
    + "and kit.id not in (select kit2.id from SexualAssaultKit kit2 "
    + "left join kit2.chainOfCustody event2 left join kit2.legalDetails details "
    + "where event2.eventFlag = 'LE_SENT_FOR_ANALYSIS' "
    + "or (details.reviewFinalized is not null and details.prosecutorAgrees = true))")
  List<SexualAssaultKit> findKitsThatNeedLeAction(LocalDate receivedDate);

  @Query(value = "select kit from SexualAssaultKit kit "
    + "join kit.chainOfCustody event "
    + "where event.eventFlag = 'LE_RECEIVED_COLLECTED_KIT' and event.eventDate between ?2 and ?1 "
    + "and kit.id not in (select kit2.id from SexualAssaultKit kit2 "
    + "left join kit2.chainOfCustody event2 left join kit2.legalDetails details "
    + "where event2.eventFlag = 'LE_SENT_FOR_ANALYSIS' "
    + "or (details.reviewFinalized is not null and details.prosecutorAgrees = true))")
  List<SexualAssaultKit> findKitsAtLeInViolation(LocalDate receivedDate, LocalDate cutOffDate);

  @Query(value = "select sak from SexualAssaultKit sak "
    + "left join sak.medicalDetails medicalDetails "
    + "left join sak.leDetails leDetails "
    + "left join sak.legalDetails legalDetails "
    + "left join sak.chainOfCustody event "
    + "where medicalDetails.collectionDate < ?1 "
    + "and leDetails.plannedDestructionDate is null "
    + "and ("
    + "(leDetails.meetsSubmissionCriteria = false and legalDetails.prosecutorAgrees = true) "
    + "or (event.eventFlag = 'LE_RECEIVED_ANALYZED_KIT')"
    + ") "
    + "and INDEX(event) = (select max(INDEX(c2)) from SexualAssaultKit sak2 join sak2.chainOfCustody c2 where sak2.id = sak.id) "
  )
  List<SexualAssaultKit> findKitsNeedDestructionDateReminder(LocalDate collectionDate);

  //-------------------------------Queries for Reports-----------------------------------------

  @Query(value = "select sak from SexualAssaultKit sak "
    + "join sak.chainOfCustody c "
    + "where c.eventFlag = 'NEW_KIT_SENT_FROM_LAB' "
    + "and c.eventDate BETWEEN ?1 AND ?2 "
    + "order by sak.currentAssignment, sak.expirationDate"
  )
  List<SexualAssaultKit> findDistributedInDateRange(LocalDate startDate, LocalDate endDate);

  @Query(value = "select sak from SexualAssaultKit sak "
    + "join sak.chainOfCustody c "
    + "left join sak.medicalDetails medicalDetails "
    + "where c.eventFlag = 'LE_RECEIVED_COLLECTED_KIT'"
    + "and c.eventDate BETWEEN ?1 AND ?2 "
    + "order by medicalDetails.requestingLeAgency, sak.serialNumber"
  )
  List<SexualAssaultKit> findReceivedByLeInDateRange(LocalDate startDate, LocalDate endDate);

  @Query(value = "select distinct sak from SexualAssaultKit sak "
    + "join sak.chainOfCustody c "
    + "left join sak.leDetails leDetails "
    + "left join sak.legalDetails legalDetails "
    + "where (leDetails.meetsSubmissionCriteria = false and legalDetails.prosecutorAgrees = true) "
    + "and legalDetails.reviewFinalized BETWEEN ?1 AND ?2 "
    + "order by sak.serialNumber"
  )
  List<SexualAssaultKit> findUnsubmittableInDateRange(LocalDate startDate, LocalDate endDate);

  @Query(value = "select sak from SexualAssaultKit sak "
    + "join sak.chainOfCustody c "
    + "left join sak.medicalDetails medicalDetails "
    + "where c.eventFlag = 'LE_SENT_FOR_ANALYSIS'"
    + "and c.eventDate BETWEEN ?1 AND ?2 "
    + "order by medicalDetails.requestingLeAgency, sak.serialNumber"
  )
  List<SexualAssaultKit> findSubmittedByDateRange(LocalDate startDate, LocalDate endDate);

  @Query(value = "select sak from SexualAssaultKit sak "
    + "left join sak.labDetails labDetails "
    + "left join sak.medicalDetails medicalDetails "
    + "where labDetails.dnaDatabaseEntry = 'YES' "
    + "and labDetails.dnaDatabaseEntryDate <> null "
    + "and labDetails.dnaDatabaseEntryDate BETWEEN ?1 AND ?2 "
    + "order by medicalDetails.requestingLeAgency, sak.serialNumber"
  )
  List<SexualAssaultKit> findEnteredInDatabaseByDateRange(LocalDate startDate, LocalDate endDate);

  @Query(value = "select sak from SexualAssaultKit sak "
    + "left join sak.labDetails labDetails "
    + "left join sak.medicalDetails medicalDetails "
    + "where sak.labDetails.dnaDatabaseHitDate <> null "
    + "and sak.labDetails.dnaDatabaseHitDate BETWEEN ?1 AND ?2 "
    + "order by medicalDetails.requestingLeAgency, sak.serialNumber"
  )
  List<SexualAssaultKit> findHitsInKitsByDateRange(LocalDate startDate, LocalDate endDate);

  @Transactional(propagation = Propagation.REQUIRES_NEW)
  @Query("FROM SexualAssaultKit WHERE id = ?1")
  @EntityGraph(value = "sak.with.events", type = EntityGraphType.LOAD)
  SexualAssaultKit findOneInNewTransaction(Long id);
}
