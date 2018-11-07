package gov.idaho.isp.saktrack.mailer;

public interface Mailer {
  void send(MailMessage message);
}