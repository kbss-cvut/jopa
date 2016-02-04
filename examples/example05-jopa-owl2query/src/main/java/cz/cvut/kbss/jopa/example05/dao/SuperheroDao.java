package cz.cvut.kbss.jopa.example05.dao;

import cz.cvut.kbss.jopa.example05.PersistenceFactory;
import cz.cvut.kbss.jopa.example05.model.Superhero;
import cz.cvut.kbss.jopa.example05.model.Vocabulary;
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

    public void persist(Superhero hero) {
        final EntityManager em = entityManager();
        try {
            em.getTransaction().begin();
            em.persist(hero);
            em.getTransaction().commit();
        } finally {
            em.close();
        }
    }

    public void persistAll(Collection<Superhero> heroes) {
        final EntityManager em = entityManager();
        try {
            em.getTransaction().begin();
            heroes.forEach(em::persist);
            em.getTransaction().commit();
        } finally {
            em.close();
        }
    }

    public List<Superhero> findAllAssociates(Superhero hero) {
        final EntityManager em = entityManager();
        try {
            final List<Superhero> result = em
                    .createNativeQuery("SELECT ?y WHERE { ?hero ?knows ?y . }", Superhero.class)
                    .setParameter("hero", hero.getUri())
                    .setParameter("knows", URI.create(Vocabulary.p_knows))
                    .getResultList();
            // Unfortunately, OWL2Query currently does not support complex patterns, so we can't use a FILTER to remove the
            // hero instance itself (the property can't be irreflexive).
            result.remove(hero);
            return result;
        } finally {
            em.close();
        }
    }

    public List<Superhero> findGoodGuys() {
        final EntityManager em = entityManager();
        try {
            return em.createNativeQuery("SELECT ?x WHERE { ?x a ?superhero; ?isGoodGuy ?goodGuy . }", Superhero.class)
                     .setParameter("superhero", URI.create(Vocabulary.Superhero))
                     .setParameter("isGoodGuy", URI.create(Vocabulary.p_goodGuy))
                     .setParameter("goodGuy", Boolean.TRUE.toString(), "en").getResultList();
        } finally {
            em.close();
        }
    }

    private EntityManager entityManager() {
        return PersistenceFactory.createEntityManager();
    }
}
