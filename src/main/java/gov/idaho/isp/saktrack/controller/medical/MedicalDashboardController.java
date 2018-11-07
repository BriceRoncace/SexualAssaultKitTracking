package gov.idaho.isp.saktrack.controller.medical;

import gov.idaho.isp.saktrack.controller.BaseOrganizationController;
import gov.idaho.isp.saktrack.domain.SexualAssaultKit;
import gov.idaho.isp.saktrack.domain.SexualAssaultKitRepository;
import gov.idaho.isp.saktrack.domain.organization.Organization;
import gov.idaho.isp.saktrack.domain.user.User;
import gov.idaho.isp.saktrack.domain.user.organization.OrganizationUserRepository;
import gov.idaho.isp.saktrack.util.CookieUtils;
import gov.idaho.isp.saktrack.util.PagingUtils;
import gov.idaho.isp.saktrack.util.SortWrapper;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;
import javax.servlet.http.HttpServletResponse;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.CookieValue;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestAttribute;
import org.springframework.web.bind.annotation.RequestParam;

@Controller
public class MedicalDashboardController extends BaseOrganizationController {
  private final SexualAssaultKitRepository sexualAssaultKitRepository;

  public MedicalDashboardController(OrganizationUserRepository organizationUserRepository, SexualAssaultKitRepository sexualAssaultKitRepository) {
    super(organizationUserRepository);
    this.sexualAssaultKitRepository = sexualAssaultKitRepository;
  }

  @GetMapping("/medical/dashboard")
  public String dashboard(@CookieValue Optional<String> medicalPagingParams,
                          @RequestAttribute Organization organization,
                          MedDashboardPageRequest pageRequests,
                          Model model,
                          @RequestAttribute User user,
                          @RequestParam Map<String,String> requestParams,
                          HttpServletResponse response) {

    if (requestParams.isEmpty() && medicalPagingParams.isPresent()) {
      return "redirect:/medical/dashboard" + medicalPagingParams.get();
    }
    model.addAttribute("organization", organization);
    model.addAttribute("incomingKits", findIncomingKits(organization, pageRequests.getIncoming()));
    model.addAttribute("inProcessKits", findInProcessKits(organization, pageRequests.getInProcess()));
    if (canAndShouldSeeUnverifiedUserMessage(organization, user)) {
      model.addAttribute("alerts", getText("new.user.verification"));
    }
    response.addCookie(CookieUtils.createEncoded("medicalPagingParams", pageRequests.getAsUrlParams()));
    return "medical/dashboard";
  }

  private Page<SexualAssaultKit> findIncomingKits(Organization org, PageRequest pageReq) {
    Function<Pageable,Page<SexualAssaultKit>> getPageFn = pr -> sexualAssaultKitRepository.findIncomingByOrganization(org.getId(), pr);
    return PagingUtils.getFirstPageIfRequestedIsBlank(getPageFn, pageReq);
  }

  private Page<SexualAssaultKit> findInProcessKits(Organization org, PageRequest pageReq) {
    Function<Pageable,Page<SexualAssaultKit>> getPageFn = pr -> sexualAssaultKitRepository.findReceivedByOrganization(org.getId(), pr);
    return PagingUtils.getFirstPageIfRequestedIsBlank(getPageFn, pageReq);
  }

  public static class MedDashboardPageRequest {
    private static final int DEFAULT_PAGE_SIZE = 20;

    private int incomingPage;
    private SortWrapper incomingSort;
    private int inProcessPage;
    private SortWrapper inProcessSort;

    public int getIncomingPage() {
      return incomingPage;
    }

    public void setIncomingPage(int incomingPage) {
      this.incomingPage = incomingPage;
    }

    public SortWrapper getIncomingSort() {
      return incomingSort;
    }

    public void setIncomingSort(SortWrapper incomingSort) {
      this.incomingSort = incomingSort;
    }

    public int getInProcessPage() {
      return inProcessPage;
    }

    public void setInProcessPage(int inProcessPage) {
      this.inProcessPage = inProcessPage;
    }

    public SortWrapper getInProcessSort() {
      return inProcessSort;
    }

    public void setInProcessSort(SortWrapper inProcessSort) {
      this.inProcessSort = inProcessSort;
    }

    public PageRequest getIncoming() {
      if (incomingSort != null) {
        return PageRequest.of(incomingPage, DEFAULT_PAGE_SIZE, incomingSort.unwrap());
      }
      return PageRequest.of(incomingPage, DEFAULT_PAGE_SIZE);
    }

    public PageRequest getInProcess() {
      if (inProcessSort != null) {
        return PageRequest.of(inProcessPage, DEFAULT_PAGE_SIZE, inProcessSort.unwrap());
      }
      return PageRequest.of(inProcessPage, DEFAULT_PAGE_SIZE);
    }

    public String getAsUrlParams() {
      StringBuilder sb = new StringBuilder();
      sb.append("?incomingPage=").append(incomingPage)
        .append("&incomingSort=").append(PagingUtils.getSingleOrderBy(incomingSort))
        .append("&inProcessPage=").append(inProcessPage)
        .append("&inProcessSort=").append(PagingUtils.getSingleOrderBy(inProcessSort));
      return sb.toString();
    }
  }
}
