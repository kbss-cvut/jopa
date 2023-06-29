/**
 * Copyright (C) 2023 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.sessions.validator;

import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;

import java.util.ArrayList;
import java.util.List;

class GeneralIntegrityConstraintsValidator extends IntegrityConstraintsValidator {

    private final List<IntegrityConstraintsValidator> validators = new ArrayList<>();

    protected void addValidator(IntegrityConstraintsValidator validator) {
        validators.add(validator);
    }

    @Override
    public void validate(Object identifier, FieldSpecification<?, ?> attribute, Object attributeValue) {
        for (IntegrityConstraintsValidator validator : validators) {
            validator.validate(identifier, attribute, attributeValue);
        }
    }
}
