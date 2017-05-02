/*
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package de.fuberlin.wiwiss.silk.plugins.aggegrator

import de.fuberlin.wiwiss.silk.linkagerule.similarity.Aggregator
import de.fuberlin.wiwiss.silk.runtime.plugin.Plugin
import de.fuberlin.wiwiss.silk.entity.Index

@Plugin(id = "max", label = "Maximum", description = "Selects the maximum value.")
case class MaximumAggregator() extends Aggregator {
  /**
   * Returns the maximum of the provided values.
   */
  override def evaluate(values: Traversable[(Int, Option[Double])]) = {
    if (values.isEmpty)
      None
    else {
      var max = Double.MinValue
      var allNil = true
      for(value <- values) {
        if(value._2.isDefined) {
          if (allNil) allNil = false
          if (value._2.get > max)
            max = value._2.get
        }
      }
      if(allNil) None
      else Some(max)
    }
  }

  /**
   * Combines two indexes into one.
   */
  //TODO change to merge?
  override def combineIndexes(index1: Index, index2: Index)= index1 disjunction index2
}