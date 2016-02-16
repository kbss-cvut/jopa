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
package cz.cvut.kbss.jopa.example05.dao;

import cz.cvut.kbss.jopa.example05.PersistenceFactory;
import cz.cvut.kbss.jopa.example05.model.Superhero;
import cz.cvut.kbss.jopa.example05.model.Vocabulary;
import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;

import java.net.URI;
import java.util.Collection;
import java.util.List;

/**
 * Data access object for persistence operations.
 */
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

    /**
     * Finds all associates of the specified superhero.
     * <p>
     * Notice that we are relying on inference, although the associates field in {@link Superhero} is not inferred. This
     * is because we wanted to be able to modify the field value.
     *
     * @param hero Superhero, whose associates we want to find
     * @return List of associates
     */
    public List<Superhero> findAllAssociates(Superhero hero) {
        final EntityManager em = entityManager();
        try {
            final List<Superhero> result = em
                    .createNativeQuery("SELECT ?y WHERE { ?hero ?knows ?y . }", Superhero.class)
                    .setParameter("hero", hero.getUri())
                    .setParameter("knows", URI.create(Vocabulary.p_knows))
                    .getResultList();
            // The property cannot be irreflexive and OWL2Query does not support FILTER, so we have to remove
            // the argument itself manually
            result.remove(hero);
            return result;
        } finally {
            em.close();
        }
    }

    /**
     * Here we are searching by value of a property that is not mapped by our object model.
     *
     * @return List of superheroes matching the 'good guy' definition
     */
    public List<Superhero> findGoodGuys() {
        final EntityManager em = entityManager();
        try {
            return em.createNativeQuery("SELECT ?x WHERE { ?x a ?superhero; ?isGoodGuy ?goodGuy . }", Superhero.class)
                     .setParameter("superhero", URI.create(Vocabulary.Superhero))
                     .setParameter("isGoodGuy", URI.create(Vocabulary.p_goodGuy))
                     // Unmapped properties are all saved as string literals, so we need to set the parameter accordingly
                     .setParameter("goodGuy", Boolean.TRUE.toString(), "en").getResultList();
        } finally {
            em.close();
        }
    }

    private EntityManager entityManager() {
        return PersistenceFactory.createEntityManager();
    }
}
