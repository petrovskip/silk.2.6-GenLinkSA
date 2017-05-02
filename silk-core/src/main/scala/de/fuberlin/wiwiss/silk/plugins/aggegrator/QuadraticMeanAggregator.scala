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

/**
 * Computes the weighted quadratic mean.
 */
@Plugin(id = "quadraticMean", label = "Euclidian distance", description = "Calculates the Euclidian distance.")
case class QuadraticMeanAggregator() extends Aggregator {
  override def evaluate(values: Traversable[(Int, Option[Double])]) = {
    if (!values.isEmpty) {
      val sqDistance = values.map { case (weight, value) => if(value.isDefined) weight * value.get * value.get else 0}.reduceLeft(_ + _)
      val totalWeights = values.map { case (weight, value) => weight }.sum

      Some(math.sqrt(sqDistance / totalWeights))
    }
    else {
      None
    }
  }

  /**
   * Combines two indexes into one.
   */
  override def combineIndexes(index1: Index, index2: Index) = index1 conjunction index2
}