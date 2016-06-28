/**
 * Copyright (C) 2016 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later
 * version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License along with this program. If not, see
 * <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.ontodriver.sesame;

import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.config.ConfigParam;
import cz.cvut.kbss.ontodriver.config.Configuration;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.sesame.config.SesameConfigParam;
import cz.cvut.kbss.ontodriver.sesame.connector.Connector;
import cz.cvut.kbss.ontodriver.sesame.connector.ConnectorFactory;
import org.junit.After;
import org.junit.BeforeClass;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.Collection;
import java.util.Iterator;

import static org.junit.Assert.assertEquals;

abstract class ListHandlerWithStorageTestBase {

    static NamedResource OWNER = NamedResource
            .create("http://krizik.felk.cvut.cz/ontologies/jopa/entities#EntityC");

    static final String LIST_PROPERTY = "http://krizik.felk.cvut.cz/ontologies/2008/6/sequences.owl#hasListProperty";
    static final String NEXT_NODE_PROPERTY = "http://krizik.felk.cvut.cz/ontologies/2008/6/sequences.owl#hasNext";

    static Configuration configuration;

    protected Connector connector;

    @BeforeClass
    public static void setUpBeforeClass() throws Exception {
        OntologyStorageProperties storageProperties = OntologyStorageProperties
                .physicalUri(URI.create("SesameListTest"))
                .driver(SesameDataSource.class.getCanonicalName())
                .build();
        configuration = new Configuration(storageProperties);

        configuration.setProperty(SesameConfigParam.USE_VOLATILE_STORAGE, Boolean.TRUE.toString());
        configuration.setProperty(SesameConfigParam.USE_INFERENCE, Boolean.FALSE.toString());
        configuration.setProperty(ConfigParam.USE_TRANSACTIONAL_ONTOLOGY, Boolean.TRUE.toString());
        configuration.setProperty(ConfigParam.ONTOLOGY_LANGUAGE, "en");
    }

    @After
    public void tearDown() throws Exception {
        connector.close();
        ConnectorFactory.getInstance().close();
        final Field openField = ConnectorFactory.getInstance().getClass().getDeclaredField("open");
        openField.setAccessible(true);
        openField.set(ConnectorFactory.getInstance(), true);
    }

    void verifyListContent(Collection<Axiom<NamedResource>> expected, Collection<Axiom<NamedResource>> actual)
            throws Exception {
        assertEquals(expected.size(), actual.size());
        // This is more explicit on failure than just containsAll
        final Iterator<Axiom<NamedResource>> itExp = expected.iterator();
        final Iterator<Axiom<NamedResource>> itAct = actual.iterator();
        while (itExp.hasNext()) {
            assertEquals(itExp.next(), itAct.next());
        }
    }
}
