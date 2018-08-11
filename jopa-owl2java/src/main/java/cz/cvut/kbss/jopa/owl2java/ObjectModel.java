package cz.cvut.kbss.jopa.owl2java;

import com.sun.codemodel.JCodeModel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;

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
            if (!result) {
                LOG.error("Unable to create target directory structure.");
            }
            codeModel.build(file);
        } catch (IOException e) {
            LOG.error("Unable to write out the generated object model.", e);
        }
    }
}
