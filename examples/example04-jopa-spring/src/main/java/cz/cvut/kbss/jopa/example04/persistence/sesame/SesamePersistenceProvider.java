/**
 * Copyright (C) 2011 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.example04.persistence.sesame;

import cz.cvut.kbss.jopa.example04.model.Student;
import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.model.EntityManagerFactory;
import org.openrdf.repository.Repository;
import org.openrdf.repository.RepositoryException;
import org.openrdf.repository.config.RepositoryConfigException;
import org.openrdf.repository.manager.RepositoryProvider;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.PropertySource;
import org.springframework.core.env.Environment;

import javax.annotation.PostConstruct;
import java.net.URI;

@Configuration
@PropertySource("classpath:config.properties")
public class SesamePersistenceProvider {

    private static final Logger LOG = LoggerFactory.getLogger(SesamePersistenceProvider.class);
    private static final String URL_PROPERTY = "repositoryUrl";

    @Autowired
    private Environment environment;

    @Autowired
    private EntityManagerFactory emf;

    private Repository repository;

    @Bean
    public Repository repository() {
        return repository;
    }

    @PostConstruct
    private void initializeStorage() {
        forceRepoInitialization();
        final String repoUrl = environment.getProperty(URL_PROPERTY);
        try {
            this.repository = RepositoryProvider.getRepository(repoUrl);
            assert repository.isInitialized();
        } catch (RepositoryException | RepositoryConfigException e) {
            LOG.error("Unable to connect to Sesame repository at " + repoUrl, e);
        }
    }

    /**
     * Force JOPA to initialize the storage so that we don't have to initialize it ourselves.
     * <p>
     * If we were to initialize the storage, we would have to create appropriate {@link
     * org.openrdf.repository.config.RepositoryConfig} for the repo, so we rather let JOPA do it for us.
     */
    private void forceRepoInitialization() {
        final EntityManager em = emf.createEntityManager();
        try {
            // The URI doesn't matter, we just need to trigger repository connection initialization
            em.find(Student.class, URI.create("http://unknown"));
        } finally {
            em.close();
        }
    }
}
