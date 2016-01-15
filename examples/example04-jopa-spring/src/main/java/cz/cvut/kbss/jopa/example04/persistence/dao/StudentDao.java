package cz.cvut.kbss.jopa.example04.persistence.dao;

import cz.cvut.kbss.jopa.example04.model.Student;
import cz.cvut.kbss.jopa.example04.model.Vocabulary;
import cz.cvut.kbss.jopa.exceptions.NoResultException;
import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.model.EntityManagerFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import java.net.URI;
import java.util.List;

@Repository
public class StudentDao {

    private static final Logger LOG = LoggerFactory.getLogger(StudentDao.class);

    @Autowired
    private EntityManagerFactory emf;

    private EntityManager entityManager() {
        return emf.createEntityManager();
    }

    public List<Student> findAll() {
        final EntityManager em = entityManager();
        try {
            return em.createNativeQuery("SELECT ?x WHERE { ?x a ?type . }", Student.class).setParameter("type",
                    URI.create(Vocabulary.URI_BASE)).getResultList();
        } finally {
            em.close();
        }
    }

    public Student findByKey(String key) {
        final EntityManager em = entityManager();
        try {
            return em.createNativeQuery("SELECT ?x WHERE { ?x ?hasKey ?key . }", Student.class)
                     .setParameter("hasKey", URI.create(Vocabulary.p_key)).setParameter("key", "key").getSingleResult();
        } catch (NoResultException e) {
            LOG.warn("Student with key {} not found.", key);
            return null;
        } finally {
            em.close();
        }
    }

    public void persist(Student student) {
        assert student != null;
        assert student.getUri() != null;

        final EntityManager em = entityManager();
        try {
            em.getTransaction().begin();
            em.persist(student);
            em.getTransaction().commit();
        } finally {
            em.close();
        }
        LOG.debug("Student {} persisted.", student);
    }

    public void delete(Student student) {
        assert student != null;

        final EntityManager em = entityManager();
        try {
            em.getTransaction().begin();
            final Student toRemove = em.merge(student);
            em.remove(toRemove);
            em.getTransaction().commit();
        } finally {
            em.close();
        }
        LOG.debug("Student {} deleted.", student);
    }
}
