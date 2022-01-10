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
package cz.cvut.kbss.ontodriver.jena.connector;

import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.RDFNode;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.Statement;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * This connector implementation supports proper inference.
 */
class SnapshotStorageConnectorWithInference extends SnapshotStorageConnector implements InferredStorageConnector {

    private final Map<String, String> reasonerConfig;

    SnapshotStorageConnectorWithInference(AbstractStorageConnector centralConnector,
                                          Map<String, String> reasonerConfig) {
        super(centralConnector);
        this.reasonerConfig = reasonerConfig;
    }

    @Override
    void snapshotCentralDataset() {
        final SnapshotStorageWithInference s = new SnapshotStorageWithInference(configuration, reasonerConfig);
        s.addCentralData(centralConnector.getStorage().getDataset());
        this.storage = s;
    }

    @Override
    public List<Statement> find(Resource subject, Property property, RDFNode value, Collection<String> contexts) {
        ensureTransactionalState();
        if (contexts.isEmpty()) {
            return ((SnapshotStorageWithInference) storage).getRawDefaultGraph()
                                                           .listStatements(subject, property, value).toList();
        } else {
            return contexts.stream().map(ctx -> ((SnapshotStorageWithInference) storage).getRawNamedGraph(ctx)
                                                                                        .listStatements(subject,
                                                                                                property, value)
                                                                                        .toList())
                           .flatMap(Collection::stream).collect(Collectors.toList());
        }
    }

    @Override
    public boolean contains(Resource subject, Property property, RDFNode value, Collection<String> contexts) {
        ensureTransactionalState();
        if (contexts.isEmpty()) {
            return ((SnapshotStorageWithInference) storage).getRawDefaultGraph().contains(subject, property, value);
        } else {
            return contexts.stream().anyMatch(c -> ((SnapshotStorageWithInference) storage).getRawNamedGraph(c)
                                                                                           .contains(subject, property,
                                                                                                   value));
        }
    }

    @Override
    public List<Statement> findWithInference(Resource subject, Property property, RDFNode value,
                                             Collection<String> contexts) {
        ensureTransactionalState();
        if (contexts.isEmpty()) {
            return storage.getDefaultGraph().listStatements(subject, property, value).toList();
        } else {
            return contexts.stream()
                           .map(ctx -> storage.getNamedGraph(ctx).listStatements(subject, property, value).toList())
                           .flatMap(Collection::stream).collect(Collectors.toList());
        }
    }

    @Override
    public boolean containsWithInference(Resource subject, Property property, RDFNode value,
                                         Collection<String> contexts) {
        ensureTransactionalState();
        if (contexts.isEmpty()) {
            return storage.getDefaultGraph().contains(subject, property, value);
        } else {
            return contexts.stream().anyMatch(c -> storage.getNamedGraph(c).contains(subject, property, value));
        }
    }

    @Override
    public boolean isConsistent(String context) {
        ensureTransactionalState();
        return ((SnapshotStorageWithInference) storage).checkConsistency(context).isValid();
    }
}
