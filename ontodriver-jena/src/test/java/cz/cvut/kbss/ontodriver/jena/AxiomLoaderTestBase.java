/**
 * Copyright (C) 2019 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.jena;

import cz.cvut.kbss.ontodriver.descriptor.AxiomDescriptor;
import cz.cvut.kbss.ontodriver.jena.environment.Generator;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import org.apache.jena.rdf.model.Resource;

import java.net.URI;
import java.util.HashMap;
import java.util.Map;

import static org.apache.jena.rdf.model.ResourceFactory.createResource;

public abstract class AxiomLoaderTestBase {

    static final NamedResource SUBJECT = NamedResource.create(Generator.generateUri());
    static final Resource SUBJECT_RES = createResource(SUBJECT.getIdentifier().toString());
    static final URI CONTEXT = Generator.generateUri();

    Map<String, Assertion> mapAssertions(AxiomDescriptor descriptor) {
        final Map<String, Assertion> map = new HashMap<>();
        descriptor.getAssertions().forEach(a -> map.put(a.getIdentifier().toString(), a));
        return map;
    }
}
