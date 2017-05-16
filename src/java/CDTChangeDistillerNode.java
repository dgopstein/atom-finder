package atom_finder;

import ch.uzh.ifi.seal.changedistiller.treedifferencing.Node;
import ch.uzh.ifi.seal.changedistiller.model.classifiers.java.JavaEntityType;
import org.eclipse.cdt.core.dom.ast.IASTNode;

public class CDTChangeDistillerNode extends Node {
  private IASTNode node;

  public CDTChangeDistillerNode(JavaEntityType type, IASTNode node) {
    super(type, node.getClass().getSimpleName());
    this.node = node;
  }

  public CDTChangeDistillerNode(IASTNode node) {
    this(JavaEntityType.WILDCARD_TYPE, node);
  }

  public IASTNode node() {
    return this.node;
  }
}
