package cz.cvut.kbss.jopa.example04.config;

import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;

@Configuration
@ComponentScan(basePackages = "cz.cvut.kbss.jopa.example04.persistence")
public class PersistenceConfig {
}
