package cz.cvut.kbss.ontodriver.owlapi;

import cz.cvut.kbss.ontodriver_new.model.Assertion;
import cz.cvut.kbss.ontodriver_new.model.Axiom;
import cz.cvut.kbss.ontodriver_new.model.NamedResource;

import java.util.Collection;
import java.util.Set;

interface AxiomLoader {

    /**
     * Loads axioms for the specified assertions (properties).
     *
     * @param subject    Axiom subject (individual)
     * @param assertions The assertions to load
     * @return Matching assertion axioms
     */
    Collection<Axiom<?>> loadAxioms(NamedResource subject, Set<Assertion> assertions);

    /**
     * Gets all property axioms.
     *
     * @param subject Property axiom subject (individual)
     * @return All available property axioms
     */
    Collection<Axiom<?>> loadPropertyAxioms(NamedResource subject);
}
