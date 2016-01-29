package cz.cvut.kbss.jopa.example05.dao;

import cz.cvut.kbss.jopa.example05.PersistenceFactory;
import cz.cvut.kbss.jopa.example05.model.Superhero;
import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;

import java.net.URI;
import java.util.Collection;
import java.util.List;

public class SuperheroDao {

    private final URI typeUri;

    public SuperheroDao() {
        final OWLClass owlClass = Superhero.class.getAnnotation(OWLClass.class);
        assert owlClass != null;
        this.typeUri = URI.create(owlClass.iri());
    }

    public List<Superhero> findAll() {
        final EntityManager em = entityManager();
        try {
            return em.createNativeQuery("SELECT ?x WHERE { ?x a ?type . }", Superhero.class)
                     .setParameter("type", typeUri).getResultList();
        } finally {
            em.close();
        }
    }

    public void persist(Superhero instance) {
        final EntityManager em = entityManager();
        try {
            em.getTransaction().begin();
            instance.generateUri();
            em.persist(instance);
            em.getTransaction().commit();
        } finally {
            em.close();
        }
    }

    public void persistAll(Collection<Superhero> instances) {
        final EntityManager em = entityManager();
        try {
            em.getTransaction().begin();
            instances.forEach(hero -> {
                hero.generateUri();
                em.persist(hero);
            });
            em.getTransaction().commit();
        } finally {
            em.close();
        }
    }

    private EntityManager entityManager() {
        return PersistenceFactory.createEntityManager();
    }
}
