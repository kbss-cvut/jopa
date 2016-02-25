/**
 * Copyright (C) 2016 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.owlapi;

import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;

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
