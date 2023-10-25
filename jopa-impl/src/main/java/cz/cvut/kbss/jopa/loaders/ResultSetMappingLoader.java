/*
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.loaders;

import cz.cvut.kbss.jopa.model.annotations.SparqlResultSetMapping;
import cz.cvut.kbss.jopa.model.annotations.SparqlResultSetMappings;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;
import java.util.function.Consumer;

/**
 * Manages {@link cz.cvut.kbss.jopa.model.annotations.SparqlResultSetMapping} instances discovered during classpath
 * processing.
 */
public class ResultSetMappingLoader implements Consumer<Class<?>> {

    private final Set<SparqlResultSetMapping> mappings = new HashSet<>();

    @Override
    public void accept(Class<?> cls) {
        final SparqlResultSetMapping mapping = cls.getDeclaredAnnotation(SparqlResultSetMapping.class);
        if (mapping != null) {
            mappings.add(mapping);
        }
        final SparqlResultSetMappings set = cls.getDeclaredAnnotation(SparqlResultSetMappings.class);
        if (set != null) {
            mappings.addAll(Arrays.asList(set.value()));
        }
    }

    public Set<SparqlResultSetMapping> getMappings() {
        return mappings;
    }
}
