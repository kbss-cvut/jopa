/**
 * Copyright (C) 2022 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.rdf4j;

import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.rdf4j.exception.Rdf4jDriverException;
import org.eclipse.rdf4j.model.Resource;

interface ListIterator {

    boolean hasNext() throws Rdf4jDriverException;

    Resource nextNode() throws Rdf4jDriverException;

    Resource currentContent() throws Rdf4jDriverException;

    Axiom<NamedResource> nextAxiom() throws Rdf4jDriverException;

    void remove() throws Rdf4jDriverException;

    void replaceCurrentWith(NamedResource newNode) throws Rdf4jDriverException;
}
