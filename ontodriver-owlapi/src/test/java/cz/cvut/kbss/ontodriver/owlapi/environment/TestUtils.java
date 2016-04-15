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
package cz.cvut.kbss.ontodriver.owlapi.environment;

import cz.cvut.kbss.ontodriver.owlapi.connector.OntologySnapshot;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.semanticweb.owlapi.reasoner.OWLReasoner;

import java.util.Random;

public class TestUtils {

    private TestUtils() {
        throw new AssertionError();
    }

    private static final Random RANDOM = new Random();

    /**
     * Initializes a real ontology, which can be used in tests. It is not saved anywhere, so once the manager is
     * discarded, the ontology with all the changes is thrown away.
     *
     * @param reasoner Reasoner to use. Can be null
     * @return Ontology snapshot
     */
    public static OntologySnapshot initRealOntology(OWLReasoner reasoner) throws Exception {
        final OWLOntologyManager manager = OWLManager.createOWLOntologyManager();
        final OWLOntology ontology = manager
                .createOntology(IRI.create("http://krizik.felk.cvut.cz/ontologies/adapterTest"));
        return new OntologySnapshot(ontology, manager, manager.getOWLDataFactory(), reasoner);
    }

    public static int randomInt(int max) {
        return RANDOM.nextInt(max);
    }
}
