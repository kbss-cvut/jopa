/**
 * Copyright (C) 2020 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.owl2java;

import com.sun.codemodel.CodeWriter;
import com.sun.codemodel.JCodeModel;
import com.sun.codemodel.writer.FileCodeWriter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;

/**
 * Wraps the generated object model, allowing to access it in memory or write it out to the file system.
 */
class ObjectModel {

    private static final Logger LOG = LoggerFactory.getLogger(ObjectModel.class);

    private final JCodeModel codeModel;

    ObjectModel(JCodeModel codeModel) {
        this.codeModel = codeModel;
    }

    JCodeModel getCodeModel() {
        return codeModel;
    }

    /**
     * Writes out the object model.
     *
     * @param targetDir Target directory for the object model
     */
    void writeModel(String targetDir) {
        try {
            final File file = new File(targetDir);
            final boolean result = file.mkdirs();
            if (!result && !file.exists()) {
                LOG.error("Unable to create target directory structure.");
            }
            // Explicitly use UTF-8 encoding to prevent issues with character encoding on different platforms
            final CodeWriter writer = new FileCodeWriter(file, false, StandardCharsets.UTF_8.toString());
            codeModel.build(writer);
        } catch (IOException e) {
            LOG.error("Unable to write out the generated object model.", e);
        }
    }
}
